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
