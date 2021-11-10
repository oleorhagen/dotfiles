import yaml
import copy
# TODO - parse args and create a CLI tool here
import sys

def strip_map(_map, components):

    # Traverse the map, and only extract the components containing a (the
    # component for now) component

    # print(yaml.dump(_map, indent=2))

    # import pdb
    # pdb.set_trace()

    res=[]
    nt = copy.deepcopy(_map)
    # in_subtree(_map, "mender", res)
    dfs(_map, components, res, nt, _map)

    # Build the tree from the results
    d = {}
    # Let's just build the tree from 2 deep
    # print(f"got the result: {res}")
    for r in res:
        # print(f"res: {r[0]}, {r[1]}")
        if d.get(r[0]):
            d[r[0]][r[1]] = copy.deepcopy(_map[r[0]][r[1]])
            continue
        d[r[0]] = {r[1]: copy.deepcopy(_map[r[0]][r[1]])}
    print(yaml.dump(d, indent=2))


def dfs(tree, elements, res, newtree, origtree, path=[], depth=0):

    # print(" "*depth + "path")
    # print(" "*depth + str(path))

    # Leaf handling
    if type(tree) in [bool]:
        return
    if type(tree) == list:
        for elem in tree:
            if elem in elements:
                l = copy.deepcopy(path)
                l.append(elem)
                res.append(l)
        return

    for node in tree:
        path.append(node)
        if type(tree) == dict:
            dfs(tree[node], elements, res, newtree, origtree, path, depth=depth+1)
        if node in elements:
            # print(f"Node {node} in elements {elements}")
            l = copy.deepcopy(path)
            res.append(l)
        path.pop()


def visualize_components(components):
    _map = get_map()

    # Collect only the wanted components
    _stripped_map = strip_map(_map, components=components)



def get_map():
    with open("/home/olepor/mendersoftware/integration/component-maps.yml") as fd:
        d = fd.read()
        components = yaml.load(d)
        return components

# Now collect all repositories into a map based on the component names
# first.

if __name__ == "__main__":
    visualize_components(sys.argv[1:])
