# fortdb
Fortran Database binary format, with Python interface, and GUI.


## Compile options

| Compiler |  Options |
| --- | --- |
| gfortran | `-ffree-form -free` |
| intel | `/free` |
  

## How to use

### Initializing

In your Fortran program, if you want to use the automated file handler (so that you don't have to supply your own file unit numbers) you must first initialize the file handler by typing

```
use fortdb
call init_fortdb
```

This sets up automatic file unit assignment.

### Creating a database file

To open a database (existing or not), declare a database object and initialize:

```
type(database) :: base

call base%initialize('filename.bin')
```

Alternatively, if you wish to specify your own file unit numbers, use

```
type(database) :: base

call base%initialize('filename.bin',11)
```

(for unit number 11, for example)


### Adding a dataset

Then, to add a dataset to a database, simply type

```
call base%add('dataset,name', data_array)
```
where data_array is any type of data currently supported, including up to 7 dimensional arrays of integer(4), real(4), integer(8), real(8), character(len=*). When a character array is used, the length of character elements is set to the longest character element.

The data type and dimension is automatically determined and recorded in the database.

### Getting a dataset

Retrieving data currently requires you know what the data type is. 

Retrieve the same dataset with

```
type(dataset) :: dset
dset=base%get('dataset,name')
```

This dataset object `dset` contains all information about the dataset. 

To get just the data,

```
print*,dset%datas_XX
```

where *XX* is one of

- `i4` for integer(kind=4)
- `r4` for real(kind=4)
- `i8` for integer(kind=8)
- `r8` for real(kind=8)
- `c`  for character(len=*)

depending on what type of data you are retrieving.

The data is automatically dimensioned the same as it was in the database, with Fortran index ordering.

### Loading existing file

If a database file already exists, you can load the database with

```
base=from_file('file.bin')
```

### Remove dataset

A dataset can be removed by name or number (by the order in which it was added to the database).

`call base%remove('dataset,name')`

or

`call base%remove(1)`

You can also remove a list of datasets, as

`call base%remove(list_of_datasets)`

where `list_of_datasets` may be

`list_of_datasets=(/'dataset1','dataset2','dataset3'/)`

for example.

### Delete database

`call base%delete`

## Python interface

The fortdb.py file provides a Python interface with the database files of the same format, with both reading a writing capability. 

To use fortdb.py is much simpler, of course:

```
  import fortdb
  file=File('file.bin')

  file+={'dataset,name':data_array} # writes data_array to file.bin. Multiple datasets may be added this way

  file['dataset,name'] # gets a numpy array full of data of correct type and dimension

  file-='dataset,name'  # removes dataset 'dataset,name' from file

```


## GUI

Just open gui/gui.py and it's self-explanatory.

It's an absolutely bare bones viewer of the database files. Currently no write capability through the GUI.

## Rules on dataset names

 - up to 32 characters
 - preceding and trailing spaces don't matter, but interspersed spaces do matter


## Special Notes
To represent a hierarchical structure, like *groups* in HDF5, consider adding a forward slash to the dataset name. Then when searching for datasets, search for forward slash(es) and segregate data appropriately.
