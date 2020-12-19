# ForGtkAdvancedTreeviewInput
This is a object oriented Fortran library to make advanced input treeview for the GUI using GTK (gtk-fortran). It is not easy to generate a treeview with different cell renderers in different rows by GTK. However, this library provides you to generate a treeview in your GUI that you can get input from the generated input treeview. You can set any cell renderer (text, check, radio or combo) for each input row. The library generates two columns of treeview. First column is name of the input and input group with pixbuf, second column is input with selected cell renderers.

In order to compile you can get gtk-fortran modules from https://github.com/vmagnin/gtk-fortran/tree/gtk3/src.
