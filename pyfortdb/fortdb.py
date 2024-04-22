"""
Python interface with fortdb Fortran binary database files.
"""
from pathlib import Path
import struct
from numpy import array,ndarray,int32,float32,float64,int64,dtype
from math import prod
from collections import defaultdict

DATASET_NAME_LENGTH=32
DATASET_MAX_DIMENSION=7
DATASET_DESCRIPTION_LENGTH=3+DATASET_MAX_DIMENSION

def readi4(obj,n=1):
    return struct.unpack('{0}i'.format(n),obj.read(4*n))

def readi8(obj,n=1):
    return struct.unpack('{0}q'.format(n),obj.read(8*n))

def readr4(obj,n=1):
    return struct.unpack('{0}f'.format(n),obj.read(4*n))

def readr8(obj,n=1):
    return struct.unpack('{0}d'.format(n),obj.read(8*n))

def readstr(obj,n=1):
    return obj.read(n).decode('ascii')

def writei4(obj,*v):
    obj.write(asbytes_i4(*v))

def writei8(obj,*v):
    obj.write(asbytes_i8(*v))

def writer4(obj,*v):
    obj.write(asbytes_r4(*v))

def writer8(obj,*v):
    obj.write(asbytes_r8(*v))

def writestr(obj,*v):
    obj.write(asbytes_str(v))

def asbytes_i4(*v):
    return struct.pack('{0}i'.format(len(v)),*v)

def asbytes_i8(*v):
    return struct.pack('{0}q'.format(len(v)),*v)

def asbytes_r4(*v):
    return struct.pack('{0}f'.format(len(v)),*v)

def asbytes_r8(*v):
    return struct.pack('{0}d'.format(len(v)),*v)

def asbytes_str(*v):
    return b''.join([vv.encode('ascii') for vv in v])

READER={
    1:readi4,
    2:readr4,
    3:readi8,
    4:readr8,
    5:readstr,
}

WRITER={
    1:writei4,
    2:writer4,
    3:writei8,
    4:writer8,
    5:writestr,
}

ASBYTES={
    1:asbytes_i4,
    2:asbytes_r4,
    3:asbytes_i8,
    4:asbytes_r8,
    5:asbytes_str,
}

class File:
    def __init__(self,file,order='F',mode='r'):
        self._file=Path(file)
        self._dataset_info=defaultdict(dict)
        self._dataset_names=[]
        self._mode=mode
        self._init_database(order=order)
        self._order=order
        self._name=file

    @property
    def datasets(self):
        return self._dataset_names

    def _init_database(self,order='F'):
        self._number_of_datasets=0
        if not self._file.exists():
            return
        with self._file.open('rb') as fobj:
            self._number_of_datasets,=readi4(fobj)
            now_position=4+1
            for i in range(self._number_of_datasets):
                dset=Dataset.from_file_positioned(fobj,order=order)
                self._dataset_names.append(dset._name)
                self._dataset_info[dset._name]={
                    'position':now_position,
                    'size':dset.size,
                    'value':dset._value,
                    'description':dset.description,
                }
                now_position+=DATASET_NAME_LENGTH+DATASET_DESCRIPTION_LENGTH*4
                now_position+=dset.size

    def __getattr__(self,a):
        return self._dataset_info[a]['value']
    def __getitem__(self,a):
        return self._dataset_info[a]['value']

    def __iadd__(self,d):
        if isinstance(d,dict):
            if self._number_of_datasets==0:
                self._number_of_datasets=len(d.items())
                with self._file.open('wb') as fobj:
                    writei4(fobj,self._number_of_datasets)
            else:
                self._number_of_datasets+=len(d.items())
                with self._file.open('r+b') as fobj:
                    writei4(fobj,self._number_of_datasets)
            for name,value in d.items():
                _value=array(value)
                _description=Description.from_value(value)
                dset=Dataset(name=name,description=_description,value=_value.T)
                self._dataset_names.append(name)
                with self._file.open('ab') as fobj:
                    now_position=fobj.tell()
                    fobj.write(dset.as_bytes())
                self._dataset_info[name]={
                    'position':now_position,
                    'size':dset.size,
                    'value':_value,
                    'description':_description,
                }

        return self

    def to_file(self,file=None):
        _file=Path(file) if file else self._file
        with _file.open('wb') as fobj:
            writei4(fobj,self._number_of_datasets)
            for name,item in self._dataset_info.items():
                dset=Dataset(name=name,description=item['description'],value=item['value'],order='F')
                fobj.write(dset.as_bytes())

    def __isub__(self,name):
        self._number_of_datasets-=1
        del self._dataset_info[name]
        self._dataset_names.remove(name)
        self.to_file()
        return self

    def __enter__(self):
        return self

    def __exit__(self ,itype, value, traceback):
        return

    def __iter__(self):
        for info in self._dataset_info.items():
            yield Dataset(name=dataset_name,description=info['description'],value=info['value'])


    @property
    def name(self):
        return self._name

class Dataset:
    def __init__(self,name=None,description=None,value=None,order='F'):
        self._name=name.strip()
        self._description=description
        val=value
        if self._description is not None:
            if self._description[1]!=5:
                val=value
            elif isinstance(self._description,Description):
                val=array(value)
                string_length=value.dtype.itemsize//dtype('U1').itemsize
                fmt=f'{{:>{string_length}s}}'
                val=array([fmt.format(v) for v in val.flatten().tolist()]).reshape(self._description.shape,order=order)
            # elif self._description[2]>0:
                # val=value
                # string_length=max([len(s) for s in val])
                # fmt=f'{{:>{string_length}s}}'
                # val=[fmt.format(v) for v in val]
            # else:
                # val=value

        else:
            val=value
        self._value=val
    @classmethod
    def from_file_positioned(cls,obj,order='F'):
        _name=readstr(obj,DATASET_NAME_LENGTH)
        _description=readi4(obj,DATASET_DESCRIPTION_LENGTH)
        _dimension=_description[2]
        _shape=_description[3:3+_dimension]
        _len=prod([e for e in _shape])
        if _description[1]:
            _value1d=READER[_description[0]](obj,_len*_description[1])
            _value1d=[_value1d[i*_description[1]:(i+1)*_description[1]] for i in range(_len)]
        else:
            _value1d=READER[_description[0]](obj,_len)
        _value=array(_value1d).reshape(_shape,order=order)
        obj=cls(name=_name,description=_description,value=_value)
        return obj
    def value_as_bytes(self):
        return ASBYTES[self._description[0]](*self._value.flatten().tolist())
    def as_bytes(self):
        fmt=f'{{:>{DATASET_NAME_LENGTH}s}}'
        ret=asbytes_str(fmt.format(self._name))
        if isinstance(self._description,Description):
            ret+=asbytes_i4(*self._description())
        else:
            ret+=asbytes_i4(*self._description)
        ret+=self.value_as_bytes()
        return ret

    @property
    def size(self):
        from math import prod
        s={1:4,2:4,3:8,4:8,5:self._description[1]}[self._description[0]]
        if self._description[2]:
            s*=prod(self.shape)
        return s

    @property
    def shape(self):
        _dimension=self._description[2]
        return self._description[3:3+_dimension]

    @property
    def description(self):
        return Description.from_description(self._description)

    @property
    def name(self):
        return self._name

class Description:
    def __init__(self,description=None):
        self.__dimension=10
        self._description=description
# 0 : type ID   1:i4, 2:r4, 3:i8, 4:r8, 5:c
# 1 : length of character if type ID is 1
# 2 : dimension up to 7
# 3-9 : dimension extents
    @classmethod
    def from_value(cls,value):
        description=None
        if isinstance(value,ndarray):
            if value.dtype==int32:
                description=[1,0,len(value.shape)]+list(value.shape)+[0]*(DATASET_MAX_DIMENSION-len(value.shape))
            elif value.dtype==float32:
                description=[2,0,len(value.shape)]+list(value.shape)+[0]*(DATASET_MAX_DIMENSION-len(value.shape))
            elif value.dtype==int64:
                description=[3,0,len(value.shape)]+list(value.shape)+[0]*(DATASET_MAX_DIMENSION-len(value.shape))
            elif value.dtype==float64:
                description=[4,0,len(value.shape)]+list(value.shape)+[0]*(DATASET_MAX_DIMENSION-len(value.shape))
            elif hasattr(value.dtype,'kind'):
                description=[5,value.dtype.itemsize//dtype('U1').itemsize,len(value.shape)]+list(value.shape)+[0]*(DATASET_MAX_DIMENSION-len(value.shape))
        elif isinstance(value,list):
            return cls.from_value(array(value))
        elif isinstance(value,int):
            description=[3,0,0]+[0]*DATASET_MAX_DIMENSION
        elif isinstance(value,float):
            description=[4,0,0]+[0]*DATASET_MAX_DIMENSION
        elif isinstance(value,str):
            description=[5,len(value),0]+[0]*DATASET_MAX_DIMENSION
        else:
            print(f'not sure how to deal with data of type {type(value)}')
        obj=cls(description)
        return obj

    @classmethod
    def from_description(cls,desc):
        if isinstance(desc,Description): obj=desc
        else: obj=cls(description=desc)
        return obj
    def __call__(self):
        return self._description
    def __getitem__(self,i):
        return self._description[i]
    @property
    def shape(self):
        _dimension=self._description[2]
        return self._description[3:3+_dimension]
