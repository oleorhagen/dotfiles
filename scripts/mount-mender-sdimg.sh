#! /bin/bash -e


usage() {
	echo >&2 "mount-mender-sdimg.sh <image>"
  echo >&2 "Options:"
  echo >&2 "--umount - Unmount the mounted partition"
	exit 0
}

if [[ -z "$1" || "$1" = "-h" || "$1" = "--help" ]]; then
  usage
fi

IMAGE="${1}"

while [[ $# -ne 0 ]]; do
  case "$1" in
    --umount)
      UMOUNT=true
      ;;
  esac
  shift 1
done

echo >&2 "Reading the addresses from $IMAGE"
start_addresses=($(fdisk -l "$IMAGE" | awk '
{if (DEVICE)
    if ($2 == "*") {
           print $3
       } else {
           print $2
       }
}
$1 == "Device" {DEVICE="True"}
'))

echo -e "start addresses:\n${start_addresses[0]}"
echo -e "start addresses:\n${start_addresses[1]}"
echo -e "start addresses:\n${start_addresses[2]}"
echo -e "start addresses:\n${start_addresses[3]}"

cat <<EOF
Which partition do you whish to mount:

0 - /boot
1 - /rootfsa
2 - /rootfsb
3 - /data
EOF


read PARTITION_NR

case ${PARTITION_NR} in
  0)
    echo >&2 "Mounting the boot partition to /mnt/boot"
    MNT=/mnt/boot/
    ;;
  1)
    echo >&2 "Mounting the /rootfsa partition to /mnt/rootfsa"
    MNT=/mnt/rootfsa/
    ;;
  2)
    echo >&2 "Mounting the /rootfsb partition to /mnt/rootfsb"
    MNT=/mnt/rootfsb/
    ;;
  3)
    echo >&2 "Mounting the /data partition to /mnt/data"
    MNT=/mnt/data/
    ;;
  *)
    echo >&2 "$i not a valid index"
    exit 2
esac
if [[ -n "${UMOUNT}" ]]; then
  echo >&2 "Unmounting ${MNT}"
  sudo umount "${MNT}"
else
  [ ! -d "${MNT}" ] && sudo mkdir -p "${MNT}"
  echo >&2 "mounting: mount -o loop,offset=$((512*${start_addresses[${PARTITION_NR}]})) ${IMAGE} ${MNT}"
  sudo mount -o loop,offset=$((512*${start_addresses[${PARTITION_NR}]}))  "${IMAGE}" "${MNT}"
fi
