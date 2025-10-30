# Create rmp files for OASIS

## Compile the code

```bash
cd src
make clean
make
```

## Create input files

```bash
cd input
python create_grids_and_masks.py
```

## Launch the creation of the rmp files

```bash
./create_rmp_files.sh <src_grid> <tgt_grid>
```

With:
- <src_grid>=atmt, wavt or ocnt
- <tgt_grid>=atmt, wavt or ocnt
