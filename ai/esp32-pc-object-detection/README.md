# esp32 and pc object detection

## Datasets

```
curl -O http://images.cocodataset.org/zips/train2017.zip
curl -O http://images.cocodataset.org/zips/val2017.zip
curl -O http://images.cocodataset.org/zips/test2017.zip
curl -O http://images.cocodataset.org/annotations/annotations_trainval2017.zip
mkdir datasets
mv *.zip datasets
cd datasets
for f in *.zip; do unzip $f.zip; done
```

## To enter dev environment

```
echo 'use flake' > .direnv # or nix shell
espup install # for the first time
```

## TODO

Look at this for ideas of how to include various cargo apps https://github.com/newAM/esp-rs-nix/blob/main/flake.nix

## Device Info
Chip type:         esp32 (revision v3.0)
Crystal frequency: 40 MHz
Flash size:        4MB
Features:          WiFi, BT, Dual Core, 240MHz, Coding Scheme None
MAC address:       c0:49:ef:bc:c9:68
App/part. size:    1,052,080/4,128,768 bytes, 25.48%
