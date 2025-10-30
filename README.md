# Create rmp files for OASIS

This repository contains a tool for generating remapping files offline for the coupled simulations using OASIS coupler. Documentation can be found [here](https://jorispianezze.github.io/documentations/documentation_mesonh_ww3_croco_coupling/index.html).

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

## License

This project is licensed under the [CeCILL-B](LICENSE) license, a French open-source license similar to BSD/MIT.
