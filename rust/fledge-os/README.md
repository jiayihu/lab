# FledgeOS

Building:

```
cargo bootimage
qemu-system-x86_64 -drive format=raw,file=target/fledge/debug/bootimage-fledge-os.bin
```
