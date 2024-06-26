import tkinter as tk
from tkinter import ttk
from pathlib import Path

import tkinter as tk
from tkinter import ttk, filedialog

import sys
sys.path.insert(0,Path(__file__).absolute().parent.parent.as_posix())

class DatasetViewer():
    def __init__(self, root,file=''):
        self.root = root
        self._file=file
        self.root.title("Dataset Viewer")

        self.datasets = {}

        self.selected_dataset = None

        self.create_menu()
        self.create_widgets()
        if self._file:
            self.open_file(self._file)

    def create_menu(self):
        self.menu_bar = tk.Menu(self.root)
        self.root.config(menu=self.menu_bar)

        self.file_menu = tk.Menu(self.menu_bar, tearoff=0)
        self.file_menu.add_command(label="Open", command=self.open_file)
        self.file_menu.add_command(label="Close", command=self.close_file)
        self.file_menu.add_command(label="Exit", command=self.destroy)
        self.menu_bar.add_cascade(label="File", menu=self.file_menu)

    def open_file(self,file=''):
        from pyfortdb import fortdb
        if not file:
            file_path = Path(filedialog.askopenfilename(filetypes=[("BIN Files", "*.bin")]))
        else:
            file_path=Path(file)
        if file_path:
            self._data_type='bin'
            with fortdb.File(file_path) as file:
                for dataset_name in file.datasets:
                    self.datasets[dataset_name]=file[dataset_name]
                    self.dataset_listbox.insert('end',dataset_name)

    def close_file(self):
        self.data_table.delete(*self.data_table.get_children())
        self.dataset_listbox.delete(0,'end')
        self.data_table.heading("#0", text="")
        self.data_table['columns']=tuple(range(2))
        for j,column in enumerate(self.data_table["columns"]):
            self.data_table.heading(j, text="")

    def destroy(self):
        self.root.destroy()

    def create_widgets(self):
        self.dataset_listbox = tk.Listbox(self.root)
        self.dataset_listbox.pack(side="left", fill="y")

        self.dataset_listbox.bind("<<ListboxSelect>>", self.on_dataset_select)

        self.data_table = ttk.Treeview(self.root)
        self.vsb=ttk.Scrollbar(self.root,orient='vertical',command=self.data_table.yview)
        self.hsb=ttk.Scrollbar(self.data_table,orient='horizontal',command=self.data_table.xview)
        self.data_table.pack(side="left", fill="both", expand=True)
        self.vsb.place(relx=1,rely=0,relheight=1,anchor='ne')
        self.hsb.place(relx=0,rely=1,relwidth=1,anchor='sw')
        self.data_table.configure(yscrollcommand=self.vsb.set,xscrollcommand=self.hsb.set)

    def on_dataset_select(self, event):
        selected_index = self.dataset_listbox.curselection()
        if selected_index:
            dataset_name = self.dataset_listbox.get(selected_index)
            self.selected_dataset = self.datasets[dataset_name]
            self.display_dataset()

    def display_dataset(self):
        from numpy import unravel_index
        self.data_table.delete(*self.data_table.get_children())

        if self._data_type=='bin':
            if not self.selected_dataset.shape:
                values=self.selected_dataset.tolist()
                values=iter([values]) # scalars
                shape=tuple((0,))
            else:
                shape=self.selected_dataset.shape
                values=iter(self.selected_dataset.flatten().tolist())

        if len(shape)==1: length_row=1
        else: length_row=shape[0]

        nrow=1
        i=0

        try:
            while (row:=[next(values) for j in range(length_row)]):

                if i==0:
                    if shape[0]>0:
                        if length_row==1:
                            header=['(*}']
                        else:
                            header=['('+','.join([f'{j+1}']+['*']*(len(shape)-1))+')' for j in range(length_row)]
                        self.data_table["columns"] = tuple(range(length_row+1))
                        self.data_table.heading("#0", text="")
                        for j, dimension in enumerate(header):
                            self.data_table.heading(j, text=dimension)
                    else:
                        header=tuple(range(length_row))
                        header=['(0)']
                        self.data_table["columns"] = tuple(range(length_row+1))
                        self.data_table.heading("#0", text="")
                        for j, dimension in enumerate(header):
                            self.data_table.heading(j, text=dimension)

                if length_row==1:
                    header=[f'({nrow})']
                else:
                    ix=unravel_index(i,shape)
                    header=','.join(['(*']+[f'{ix[j+1]}' for j in range(len(shape)-1)])+')'
                if (i+1)%2:
                    self.data_table.insert("", "end", text=header, values=row, tag='gray')
                else:
                    self.data_table.insert("", "end", text=header, values=row, tag='offwhite')
                i+=1
                if (i+2)%length_row==0: nrow+=1
                        
        except StopIteration:
            pass
        self.data_table.tag_configure('gray', background='#edeffe')
        self.data_table.tag_configure('offwhite', background='#efefdf')
        for j in range(len(header)):
            self.data_table.column(j)


if __name__ == "__main__":
    import argparse
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--file',type=str,help='optional file',default='')
    args=parser.parse_args()
    root = tk.Tk()
    app = DatasetViewer(root,file=args.file)
    root.mainloop()
