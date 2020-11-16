#! /bin/bash -e


usage() {
	echo >&2 "mount-mender-sdimg.sh <image>"
  echo >&2 "Options:"
  echo >&2 "--umount - Unmount the mounted partition"
	exit 0
}

if [[ -z "$1" ]]; then
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
start_addresses=$(fdisk -l "$IMAGE" | awk 'BEGIN {DEVICE=false}
$1 == "Device" {DEVICE=true }
{if (DEVICE == true)
    if ($2 == "*")
       printf $3
    else
       print $2
}')

echo "start addresses: $start_addresses"
exit 1

cat <<EOF
Which partition do you whish to mount:

1 - /boot
2 - /rootfsa
3 - /rootfsb
4 - /data
EOF
read PARTITION_NR
for start_address in ${start_addresses}; do
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
		echo >&2 "mounting: mount -o loop,offset=$((512*start_address)) ${IMAGE} ${MNT}"
		sudo mount -o loop,offset=$((512*start_address))  "${IMAGE}" "${MNT}"
	fi
done
