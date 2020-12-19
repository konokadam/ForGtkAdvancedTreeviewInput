! Copyright (C) 2011
! Free Software Foundation, Inc.


! This is free software; you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation; either version 3, or (at your option)
! any later version.

! This software is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.

! Under Section 7 of GPL version 3, you are granted additional
! permissions described in the GCC Runtime Library Exception, version
! 3.1, as published by the Free Software Foundation.

! You should have received a copy of the GNU General Public License along with
! this program; see the files COPYING3 and COPYING.RUNTIME respectively.
! If not, see <http://www.gnu.org/licenses/>.


!!!Developed by H. E. Konokman

module advanced_tree_input_class

    use, intrinsic :: iso_c_binding, only: c_int, c_char, c_null_char, c_ptr
    use :: gtk, only: gtk_builder_get_object
    use :: gtk_hl_tree
    use :: gtk_hl_chooser

    implicit none
    type, abstract :: group_input_abstract_type
        character(len=20) :: tag
        character(len=20) :: name
        type(c_ptr) :: pixbuf
        integer(c_int), allocatable :: row(:)
    contains
        procedure(render_int), deferred :: render
    end type

    type :: group_input_container_type
        class(group_input_abstract_type), pointer :: p => null()
    end type

    type, extends(group_input_abstract_type) :: group_type
        type(group_input_container_type), allocatable :: item(:)
    contains
        !        procedure :: construct => group_constructor
        procedure :: render => render_group
    end type

    type, extends(group_input_abstract_type) :: input_type
        integer(c_int) :: col_visible
        integer(c_int) :: col_input
    contains
        !        procedure :: construct => input_constructor
        procedure :: render => render_input
    end type

    abstract interface
        subroutine render_int(item, tree, pixbuf)
            import :: group_input_abstract_type, c_ptr, c_int
            class(group_input_abstract_type), intent(in) :: item
            type(c_ptr), intent(in) :: tree
            type(c_ptr), intent(in), optional :: pixbuf
        end subroutine
    end interface

    !!!Group constructor
    interface group
        module procedure group_constructor
    end interface
    !!!Input constructor
    interface input
        module procedure input_constructor
    end interface


    enum, bind(c)
        enumerator :: COL_PIXBUF_NAME
        enumerator :: COL_TEXT_NAME
        enumerator :: COL_TEXT_INPUT
        enumerator :: COL_COMBO_INPUT
        enumerator :: COL_SPIN_INPUT
        enumerator :: COL_CHECK_INPUT
        enumerator :: COL_RADIO_INPUT
        enumerator :: COL_PROGRESS_INPUT
        enumerator :: COL_PIXBUF_INPUT
        enumerator :: COL_VISIBLE_TEXT
        enumerator :: COL_VISIBLE_COMBO
        enumerator :: COL_VISIBLE_SPIN
        enumerator :: COL_VISIBLE_CHECK
        enumerator :: COL_VISIBLE_RADIO
        enumerator :: COL_VISIBLE_PROGRESS
        enumerator :: COL_VISIBLE_PIXBUF
        enumerator :: NUM_COLS
    end enum

    enum, bind(c)
        enumerator :: GROUP_TYPE_
        enumerator :: INPUT_TYPE_
    end enum

    type(c_ptr) :: treeview_input
    type(c_ptr) :: treestore_input
    type(c_ptr) :: cell_renderer_combo_input

    type(group_input_container_type), allocatable :: item(:)

    interface
        subroutine match_tag_with_input_int(tag, input)
        character(*), intent(in) :: tag
        class(*), intent(in) :: input
        end subroutine
    end interface

    procedure(match_tag_with_input_int), pointer :: match_tag_with_input => null()

contains

    subroutine initialize_inputs(match_tag_with_input_)

        procedure(match_tag_with_input_int) :: match_tag_with_input_

        match_tag_with_input => match_tag_with_input_

    end subroutine

    !    subroutine group_or_input(item, type, row, name, pixbuf, renderer_type)
    function group_or_input(type, row, builder, tag, name, pixbuf, renderer_type, list_combo) result(item)

        class(group_input_abstract_type), pointer :: item
        integer(c_int), intent(in) :: type
        integer, intent(in) :: row(:)
        character(*), intent(in), optional :: tag
        type(c_ptr), optional :: builder
        character(*), intent(in), optional :: name
        type(c_ptr), intent(in), optional :: pixbuf
        integer(c_int), intent(in), optional :: renderer_type
        character(*), intent(in), optional :: list_combo(:)

        select case(type)
            case(GROUP_TYPE_)
                allocate(group_type :: item)
                select type(item)
                    type is(group_type)
                        !                call item%construct(row, name=name, pixbuf=pixbuf)
                        item = group(row, name=name, pixbuf=pixbuf)
                end select
            case(INPUT_TYPE_)
                allocate(input_type :: item)
                select type(item)
                    type is(input_type)
                        !                call item%construct(row, name=name, pixbuf=pixbuf, renderer=renderer)
                        item = input(builder, row, tag=tag, name=name, pixbuf=pixbuf, &
                            & renderer_type=renderer_type, list_combo=list_combo)
                end select
            case default
                allocate(input_type :: item)
                select type(item)
                    type is(input_type)
                        !                call item%construct(row, name=name, pixbuf=pixbuf, renderer=renderer)
                        item = input(builder, row, tag=tag, name=name, pixbuf=pixbuf, &
                            & renderer_type=renderer_type, list_combo=list_combo)
                end select
        end select

    end function
    !    end subroutine

    !    subroutine group_constructor(item, row, name, pixbuf)
    function group_constructor(row, tag, name, pixbuf) result(item)

        type(group_type) :: item
        integer, intent(in) :: row(:)
        character(*), intent(in), optional :: tag
        character(*), intent(in), optional :: name
        type(c_ptr), intent(in), optional :: pixbuf

        integer :: i

        allocate(item%row(size(row)))
        do i = 1, size(row)
            item%row(i) = int(row(i), c_int)
        end do

        if(present(tag)) item%tag = tag
        if(present(name)) item%name = name
        if(present(pixbuf)) item%pixbuf = pixbuf

    end function
    !    end subroutine

    !    subroutine input_constructor(item, row, name, renderer, pixbuf)
    function input_constructor(builder, row, tag, name, renderer_type, pixbuf, list_combo) result(item)

        type(input_type) :: item
        type(c_ptr) :: builder
        integer, intent(in) :: row(:)
        character(*), intent(in), optional :: tag
        character(*), intent(in), optional :: name
        integer(c_int), intent(in), optional :: renderer_type
        type(c_ptr), intent(in), optional :: pixbuf
        character(*), intent(in), optional :: list_combo(:)

        type(c_ptr) :: liststore_combo
        type(c_ptr) :: renderer
        integer(kind=type_kind), allocatable, target :: valtype(:)
        integer :: i
        type(c_ptr) :: col, rlist
        type(gvalue), target :: modelv, stringv, entryv
        type(c_ptr) :: pmodel, model, pstring, pentry
        logical :: iappend
        integer(kind=c_int) :: nvals, valid
        type(gtktreeiter), target :: iter

        allocate(item%row(size(row)))
        do i = 1, size(row)
            item%row(i) = int(row(i), c_int)
        end do

        if(present(tag)) item%tag = tag
        if(present(name)) item%name = name
        if(present(pixbuf)) item%pixbuf = pixbuf
        if(present(renderer_type)) then
            select case(renderer_type)
                case(COL_TEXT_INPUT)
                    item%col_visible = COL_VISIBLE_TEXT
                    item%col_input = COL_TEXT_INPUT
                case(COL_CHECK_INPUT)
                    item%col_visible = COL_VISIBLE_CHECK
                    item%col_input = COL_CHECK_INPUT
                case(COL_RADIO_INPUT)
                    item%col_visible = COL_VISIBLE_RADIO
                    item%col_input = COL_RADIO_INPUT
                case(COL_COMBO_INPUT)
                    item%col_visible = COL_VISIBLE_COMBO
                    item%col_input = COL_COMBO_INPUT
                    cell_renderer_combo_input = gtk_builder_get_object(builder, "cell_renderer_combo_input"//c_null_char)
                    allocate(valtype(1))
                    valtype = [G_TYPE_STRING]
                    liststore_combo = gtk_list_store_newv(1_c_int, c_loc(valtype))
                    call hl_gtk_tree_combo_set_model(cell_renderer_combo_input, liststore_combo)
                    !                    call hl_gtk_list_tree_combo_model_config(treeview_input, COL_COMBO_INPUT, &
                        !                    & vals=list_combo, append=TRUE, has_entry=FALSE)
                    !                    iappend = .false.
                    !                    ! Find the renderer for the column
                    !                    col = gtk_tree_view_get_column(treeview_input, COL_COMBO_INPUT)
                    !                    print*, 1
                    !                    read*,
                    !                    rlist = gtk_cell_layout_get_cells(col)
                    !                    print*, 2
                    !                    read*,
                    !                    renderer = g_list_nth_data(rlist, 0_c_int)
                    !                    call g_list_free(rlist)
                    !                    print*, 3
                    !                    read*,
                    !
                    !                    ! Find the model for the combobox
                    !                    pmodel = c_loc(modelv)
                    !                    pmodel = g_value_init(pmodel, gtk_tree_model_get_type())
                    !                    call g_object_get_property(renderer, "model"//c_null_char, pmodel)
                    !                    model = g_value_get_object(pmodel)
                    !
                    pstring = c_loc(stringv)
                    pstring = g_value_init(pstring, G_TYPE_STRING)

                    if (present(list_combo)) then
                        do i = 1, size(list_combo)
                            call clear_gtktreeiter(iter)
                            call gtk_list_store_append(liststore_combo, c_loc(iter))
                            call g_value_set_string(pstring, trim(list_combo(i))//c_null_char)
                            call gtk_list_store_set_value(liststore_combo, c_loc(iter), 0_c_int, pstring)
                        end do
                    end if
                case(COL_SPIN_INPUT)
                    item%col_visible = COL_VISIBLE_SPIN
                    item%col_input = COL_SPIN_INPUT
            end select
        end if


    end function
    !    end subroutine

    subroutine render_group(item, tree, pixbuf)

        class(group_type), intent(in) :: item
        type(c_ptr), intent(in) :: tree
        type(c_ptr), intent(in), optional :: pixbuf

        !!!Set pixbuf
        if(present(pixbuf)) call hl_gtk_tree_set_cell(tree, row=item%row, col=COL_PIXBUF_NAME, &
            & pbvalue=pixbuf)
        !!!Set name
        call hl_gtk_tree_set_cell(tree, row=item%row, col=COL_TEXT_NAME, &
            & svalue=trim(item%name)//c_null_char)

    end subroutine

    subroutine render_input(item, tree, pixbuf)

        class(input_type), intent(in) :: item
        type(c_ptr), intent(in) :: tree
        type(c_ptr), intent(in), optional :: pixbuf

        !!!Set pixbuf
        if(present(pixbuf)) call hl_gtk_tree_set_cell(tree, row=item%row, col=COL_PIXBUF_NAME, &
            & pbvalue=pixbuf)
        !!!Set name
        call hl_gtk_tree_set_cell(tree, row=item%row, col=COL_TEXT_NAME, &
            & svalue=trim(item%name)//c_null_char)
        !!!Set renderer visible
        call hl_gtk_tree_set_cell(tree, row=item%row, col=item%col_visible, &
            & lvalue=TRUE)

    end subroutine

    recursive subroutine construct_input_tree(item, tree)

        type(group_input_container_type), intent(in) :: item(:)
        type(c_ptr) :: tree
        integer :: i
        integer :: i1
        character(512) :: input_text
        integer(c_int) :: input_boolean


        do i = 1, size(item)
            !!!Insert the tree row
            call hl_gtk_tree_ins(tree, row=item(i)%p%row)

            call item(i)%p%render(tree, item(i)%p%pixbuf)

            select type(p => item(i)%p)
                type is(group_type)
                    if(allocated(p%item)) then
                        if(size(p%item) > 0) then
                            call construct_input_tree(p%item, tree)
                        end if
                    end if
            end select
        enddo

    end subroutine

    !!!Function gives error
    recursive subroutine item_tag(item, row, tag)

        type(group_input_container_type), intent(inout) :: item(:)
        integer(c_int), intent(in) :: row(:)
        character(50), intent(out) :: tag
        integer :: i, ir, s1, s2

        do i = 1, size(item)
            if_row: if(size(item(i)%p%row) == size(row)) then
                do ir = 1, size(row)
                    if(item(i)%p%row(ir) /= row(ir)) exit if_row
                end do

                tag = trim(item(i)%p%tag)
                return
            end if if_row

            select type(p => item(i)%p)
                type is(group_type)
                    if(allocated(p%item)) then
                        if(size(p%item) > 0) then
                            call item_tag(p%item, row, tag)
                        end if
                    end if
            end select
        enddo

    end subroutine

    subroutine check_toggled(renderer, path, gdata) bind(c)

        type(c_ptr), value :: renderer, path, gdata

        integer(kind=c_int), pointer :: icol
        type(c_ptr) :: pcol, list
        logical :: state
        character(50), allocatable :: kelimeler(:)
        integer(c_int), allocatable :: row(:)
        integer :: i, rw
        character(50) :: tag

        row = row_from_path(path)

        state = c_f_logical(gtk_cell_renderer_toggle_get_active(renderer))

        call hl_gtk_tree_set_cell(treeview_input, row=row, col=COL_CHECK_INPUT, &
            & logvalue= .not. state)

        call item_tag(item, row, tag)
        call match_tag_with_input(tag, .not. state)

        deallocate(row)

    end subroutine check_toggled

    subroutine text_edited(widget, path, text, gdata) bind(c)

        type(c_ptr), value :: widget, path, text, gdata

        character(len=500) :: ftext
        integer(c_int), allocatable :: row(:)
        character(50) :: tag

        call c_f_string(text, ftext)

        row = row_from_path(path)
        call hl_gtk_tree_set_cell(treeview_input, row=row, col=COL_TEXT_INPUT, svalue=trim(ftext))

        call item_tag(item, row, tag)
        call match_tag_with_input(tag, trim(ftext))

        deallocate(row)

    end subroutine

    subroutine combo_changed(renderer, path, iter, gdata) bind(c)

        type(c_ptr), value :: renderer, path, iter, gdata

        character(len=200) :: fpath
        character(c_char) :: svalue

        call c_f_string(path, fpath)

    end subroutine

    function row_from_path(path) result(row)

        type(c_ptr), intent(in) :: path
        integer(c_int), allocatable :: row(:)
        character(len=200) :: fpath
        integer :: i, n


        call c_f_string(path, fpath)

        n = 0
        do i = 1, len_trim(fpath)
            if (fpath(i:i) == ":") then
                n = n+1
                fpath(i:i) = ' '   ! : is not a separator for a Fortran read
            end if
        end do
        allocate(row(n+1))
        read(fpath, *) row

    end function


    subroutine combo_edited(renderer, path, text, gdata) bind(c)

        type(c_ptr), value :: renderer, path, text, gdata

        character(len=200) :: ftext
        integer(c_int), allocatable :: row(:)
        character(50) :: tag

        call c_f_string(text, ftext)

        row = row_from_path(path)

        call hl_gtk_tree_set_cell(treeview_input, row=row, col=COL_COMBO_INPUT, svalue=trim(ftext)//c_null_char)


        call item_tag(item, row, tag)
        call match_tag_with_input(tag, trim(ftext))

        deallocate(row)

    end subroutine combo_edited

    subroutine spin_edited(renderer, path, text, gdata) bind(c)

        type(c_ptr), value :: renderer, path, text, gdata

        character(len=200) :: fpath, ftext
        integer(c_int), allocatable :: row(:)
        character(50) :: tag

        call c_f_string(text, ftext)
        call c_f_string(path, fpath)

        row = row_from_path(path)

        call hl_gtk_tree_set_cell(treeview_input, row=row, col=COL_SPIN_INPUT, svalue=trim(ftext))!//c_null_char)

        call item_tag(item, row, tag)
        call match_tag_with_input(tag, trim(ftext))

        deallocate(row)

    end subroutine spin_edited

    subroutine radio_toggled(renderer, path, gdata) bind(c)
    
        type(c_ptr), value :: renderer, path, gdata

        character(len=200) :: fpath, fpath1
        integer(kind=c_int), allocatable, dimension(:) :: irow, jrow
        integer :: i, n
        type(c_ptr) :: pcol, ipath
        logical :: state
        type(gtktreeiter), target :: iter, piter
        integer(kind=c_int) :: valid
        integer(c_int), allocatable :: row(:)
        character(50) :: tag

        call convert_c_string(path, fpath)
        fpath1 = fpath
        n = 0
        do i = 1, len_trim(fpath)
            if (fpath(i:i) == ":") then
                n = n+1
                fpath(i:i) = ' '   ! : is not a separator for a Fortran read
            end if
        end do
        allocate(irow(n+1))
        read(fpath, *) irow

        state = c_f_logical(gtk_cell_renderer_toggle_get_active(renderer))
        if (state) return ! Don't act on a release of a selected toggle

        ! Clear all the siblings of the chosen
        valid = gtk_tree_model_get_iter_from_string(treestore_input, c_loc(iter), trim(fpath1)//c_null_char)
        !        valid = gtk_tree_model_get_iter_from_string(treestore_input, c_loc(iter), "0:2:0"//c_null_char)

        valid = gtk_tree_model_iter_parent (treestore_input, c_loc(piter), c_loc(iter))

        call clear_gtktreeiter(iter)

        if (c_f_logical(valid)) then
            valid = gtk_tree_model_iter_children (treestore_input, c_loc(iter),&
                & c_loc(piter))

        else
            valid = gtk_tree_model_iter_children (treestore_input, c_loc(iter),&
                & c_null_ptr)
        end if

        do
            ipath = gtk_tree_model_get_string_from_iter (treestore_input, c_loc(iter))
            call convert_c_string(ipath, fpath)
            n = 0
            do i = 1, len_trim(fpath)
                if (fpath(i:i) == ":") then
                    n = n+1
                    fpath(i:i) = ' '   ! : is not a separator for a Fortran read
                end if
            end do
            allocate(jrow(n+1))
            read(fpath, *) jrow
            call hl_gtk_tree_set_cell(treeview_input, jrow, COL_RADIO_INPUT, logvalue=.false.)
            deallocate(jrow)
            valid = gtk_tree_model_iter_next (treestore_input, c_loc(iter))
            if (.not. c_f_logical(valid)) exit
        end do
        call hl_gtk_tree_set_cell(treeview_input, irow, COL_RADIO_INPUT, &
            & logvalue= .true.)

        row = row_from_path(path)
        call item_tag(item, row, tag)
        call match_tag_with_input(tag, .true.)

        deallocate(row)

    end subroutine radio_toggled


    subroutine connect_signals_treeview_input(object, signal_name, h_name)

        type(c_ptr), value                     :: object         !object to connect a signal to
        character(kind=c_char), dimension(*)   :: signal_name    !name of the signal
        character(*), intent(in) :: h_name

        select case (h_name)
            case ("text_edited")
                call g_signal_connect (object, signal_name, c_funloc(text_edited))
            case ("combo_changed")
                call g_signal_connect (object, signal_name, c_funloc(combo_changed))
            case ("combo_edited")
                call g_signal_connect (object, signal_name, c_funloc(combo_edited))
            case ("spin_edited")
                call g_signal_connect (object, signal_name, c_funloc(spin_edited))
            case ("check_toggled")
                call g_signal_connect (object, signal_name, c_funloc(check_toggled))
            case ("radio_toggled")
                call g_signal_connect (object, signal_name, c_funloc(radio_toggled))
        end select

    end subroutine

end module
