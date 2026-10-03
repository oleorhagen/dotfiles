"""
KiCad Action Plugin: Open Mask Over Net

Adds a toolbar/menu action to the PCB Editor. When run, it asks which net
to expose, then mirrors every track on that net (straight or curved, on
F.Cu or B.Cu) as a matching graphic line/arc on F.Mask / B.Mask -- opening
the solder mask directly over those wires.

Asks for an expansion margin (mm) added to each side of the track width,
so the opening can be wider than the bare copper. Safe to re-run: it
matches mask-layer lines to tracks by position, so running it again after
adding more wire (or with a different margin) only adds new openings and
resizes existing ones -- it won't create duplicates.

Install location (already in place):
    ~/.local/share/kicad/10.0/scripting/plugins/open_mask_over_net.py

After editing this file, use Tools -> External Plugins -> Refresh Plugins
in the PCB Editor to pick up changes.
"""

import pcbnew
import wx

TRACK_LAYER_TO_MASK_LAYER = {
    pcbnew.F_Cu: pcbnew.F_Mask,
    pcbnew.B_Cu: pcbnew.B_Mask,
}


def _point(vec):
    return (vec.x, vec.y)


def _segment_key(layer, start, end):
    # Direction-agnostic: a straight line looks the same drawn either way.
    return (layer, "line", frozenset((start, end)))


def _arc_key(layer, start, mid, end):
    # Arcs keep their point order -- reversing start/end changes the curve.
    return (layer, "arc", start, mid, end)


def _existing_mask_shapes(board):
    """{key: shape} for graphic lines/arcs already on F.Mask/B.Mask."""
    existing = {}
    for drawing in board.GetDrawings():
        if not isinstance(drawing, pcbnew.PCB_SHAPE):
            continue
        if drawing.GetLayer() not in (pcbnew.F_Mask, pcbnew.B_Mask):
            continue
        start = _point(drawing.GetStart())
        end = _point(drawing.GetEnd())
        if drawing.GetShape() == pcbnew.SHAPE_T_SEGMENT:
            key = _segment_key(drawing.GetLayer(), start, end)
        elif drawing.GetShape() == pcbnew.SHAPE_T_ARC:
            mid = _point(drawing.GetArcMid())
            key = _arc_key(drawing.GetLayer(), start, mid, end)
        else:
            continue
        existing[key] = drawing
    return existing


def _net_names(board):
    names = set()
    for netcode, netinfo in board.GetNetInfo().NetsByNetcode().items():
        name = netinfo.GetNetname()
        if name:
            names.add(name)
    return sorted(names)


class OpenMaskOverNet(pcbnew.ActionPlugin):
    def defaults(self):
        self.name = "Open Mask Over Net"
        self.category = "Modify PCB"
        self.description = "Open the solder mask directly over every track on a chosen net"
        self.show_toolbar_button = True
        self.icon_file_name = ""

    def Run(self):
        board = pcbnew.GetBoard()

        net_names = _net_names(board)
        if not net_names:
            wx.MessageBox("No nets found on this board.", "Open Mask Over Net")
            return

        dlg = wx.SingleChoiceDialog(
            None, "Open the solder mask over which net?", "Open Mask Over Net", net_names
        )
        if dlg.ShowModal() != wx.ID_OK:
            dlg.Destroy()
            return
        net_name = dlg.GetStringSelection()
        dlg.Destroy()

        margin_dlg = wx.TextEntryDialog(
            None,
            "Expansion margin, added to each side of the track width (mm):",
            "Open Mask Over Net",
            "0.1",
        )
        if margin_dlg.ShowModal() != wx.ID_OK:
            margin_dlg.Destroy()
            return
        margin_text = margin_dlg.GetValue()
        margin_dlg.Destroy()
        try:
            margin_mm = float(margin_text)
        except ValueError:
            wx.MessageBox(f"'{margin_text}' isn't a number.", "Open Mask Over Net")
            return
        margin_iu = pcbnew.FromMM(margin_mm)

        existing = _existing_mask_shapes(board)
        added = 0
        resized = 0

        for track in board.GetTracks():
            if track.GetNetname() != net_name:
                continue
            is_arc = track.Type() == pcbnew.PCB_ARC_T
            if not is_arc and track.Type() != pcbnew.PCB_TRACE_T:
                continue  # ignore vias etc.

            mask_layer = TRACK_LAYER_TO_MASK_LAYER.get(track.GetLayer())
            if mask_layer is None:
                continue  # track isn't on F.Cu/B.Cu

            start = _point(track.GetStart())
            end = _point(track.GetEnd())
            target_width = track.GetWidth() + 2 * margin_iu

            if is_arc:
                mid = _point(track.GetMid())
                key = _arc_key(mask_layer, start, mid, end)
            else:
                key = _segment_key(mask_layer, start, end)

            existing_shape = existing.get(key)
            if existing_shape is not None:
                if existing_shape.GetWidth() != target_width:
                    existing_shape.SetWidth(target_width)
                    resized += 1
                continue

            shape = pcbnew.PCB_SHAPE(board)
            if is_arc:
                shape.SetShape(pcbnew.SHAPE_T_ARC)
                shape.SetArcGeometry(track.GetStart(), track.GetMid(), track.GetEnd())
            else:
                shape.SetShape(pcbnew.SHAPE_T_SEGMENT)
                shape.SetStart(track.GetStart())
                shape.SetEnd(track.GetEnd())
            shape.SetWidth(target_width)
            shape.SetLayer(mask_layer)
            board.Add(shape)

            existing[key] = shape
            added += 1

        pcbnew.Refresh()

        wx.MessageBox(
            f"Net '{net_name}': added {added}, resized {resized} mask opening(s).",
            "Open Mask Over Net",
        )


OpenMaskOverNet().register()
