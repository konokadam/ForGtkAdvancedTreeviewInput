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


!!!H. E. Konokman
!!
!  19.12.2020
module handlers

    use iso_c_binding
    use g, only: g_free
    use gdk_pixbuf, only: gdk_pixbuf_get_type, gdk_pixbuf_new
    use gtk, only: gtk_main_quit, &
        & gtk_builder_new, gtk_builder_add_from_file, gtk_builder_get_object, &
        & gtk_builder_connect_signals_full, &
        & gtk_tree_view_expand_all, gtk_tree_view_set_enable_tree_lines,&
        & gtk_widget_show, gtk_widget_show_all, gtk_window_maximize, gtk_window_new, &
        & gtk_tree_view_set_grid_lines, gtk_tree_view_collapse_all, gtk_builder_connect_signals, &
        & GTK_POS_LEFT

    use :: gtk_hl_container
    use :: gtk_hl_chooser
    use :: gtk_hl_tree
    use :: gdk_pixbuf_hl
    use :: gtk_hl_entry

    use :: advanced_tree_input_class
    use :: params


    implicit none

    type(c_ptr) :: builder
    type(c_ptr) :: window
    type(c_ptr) :: pixbuf_group, pixbuf_input

    integer(kind=c_int) :: run_status = TRUE

contains

    subroutine destroy_event_window(widget, gdata) bind(c)

        type(c_ptr), value :: widget, gdata

        call gtk_main_quit()
        run_status = FALSE

    end subroutine destroy_event_window

    subroutine exit_button_release_event_cb(widget, gdata) bind(c)

        type(c_ptr), value :: widget, gdata

        call gtk_main_quit()

    end subroutine exit_button_release_event_cb

    ! delete event
    function delete_event_window(widget, event, gdata) result(ret)  bind(c)

        use iso_c_binding, only: c_ptr, c_bool
        integer(c_int)     :: ret
        type(c_ptr), value :: widget, event, gdata

        ret = FALSE

    end function delete_event_window


    subroutine show_window()

        call gtk_widget_show(window)

    end subroutine

    subroutine tree_changed(widget, gdata) bind(c)

        type(c_ptr), value :: widget, gdata

    end subroutine

    subroutine button_expandall_clicked(widget, gdata) bind(c)

        type(c_ptr), value :: widget, gdata

        call gtk_tree_view_expand_all(treeview_input)

    end subroutine button_expandall_clicked

    subroutine button_collapseall_clicked(widget, gdata) bind(c)

        type(c_ptr), value :: widget, gdata

        call gtk_tree_view_collapse_all(treeview_input)

    end subroutine button_collapseall_clicked

    ! String routine from C_interface_module by Joseph M. Krahn
    ! http://fortranwiki.org/fortran/show/c_interface_module
    ! Copy a C string, passed as a char-array reference, to a Fortran string.
    subroutine c_f_string_chars(c_string, f_string)
        ! Helper function
        use iso_c_binding
        implicit none
        character(len=1,kind=c_char), intent(in) :: c_string(*)
        character(len=*), intent(out) :: f_string
        integer :: i
        i=1
        do while(c_string(i)/=c_null_char .and. i<=len(f_string))
            f_string(i:i) = c_string(i)
            i=i+1
        end do
        if (i<=len(f_string)) f_string(i:) = ' '
    end subroutine c_f_string_chars

    !-----------------------------------------
    subroutine connect_signals(builder, object, signal_name, handler_name, connect_object, flags, user_data) bind(c)
        type(c_ptr), value                     :: builder        !a GtkBuilder
        type(c_ptr), value                     :: object         !object to connect a signal to
        character(kind=c_char), dimension(*)   :: signal_name    !name of the signal
        character(kind=c_char), dimension(*), target   :: handler_name   !name of the handler
        type(c_ptr), value                     :: connect_object !a GObject, if non-NULL, use g_signal_connect_object()
        integer(c_int), value                  :: flags          !GConnectFlags to use
        type(c_ptr), value                     :: user_data         !user data

        character(len=32)                      :: h_name

        call c_f_string_chars(handler_name, h_name)
        print *, "Connect signal for = "//h_name

        select case (h_name)
            case ("destroy-event_window")
                call g_signal_connect (object, signal_name, c_funloc(destroy_event_window))
            case ("delete-event_window")
                call g_signal_connect (object, signal_name, c_funloc(delete_event_window))
            case ("button_expandall_clicked")
                call g_signal_connect (object, signal_name, c_funloc(button_expandall_clicked))
            case ("button_collapseall_clicked")
                call g_signal_connect (object, signal_name, c_funloc(button_collapseall_clicked))
!            case default
!                print *, "Unknow handler = "//h_name
!                stop "Program terminated"
        end select

        call connect_signals_treeview_input(object, signal_name, h_name)

    end subroutine


    subroutine create_window()

        implicit none
        type(c_ptr) :: error
        integer(c_int) :: guint


        !!!Load GUI into builder
        builder = gtk_builder_new()
        error = c_null_ptr
        guint = gtk_builder_add_from_file(builder, "ForGtkAdvancedTreeviewInput.glade"//c_null_char, error)
        if (guint == 0) then
            print *, "Could not open ForGtkAdvancedTreeviewInput.glade"
            stop "Program terminated"
        end if

        window = gtk_builder_get_object(builder, "window"//c_null_char)

        treeview_input = gtk_builder_get_object(builder, "treeview_input"//c_null_char)
        treestore_input = gtk_builder_get_object(builder, "treestore_input"//c_null_char)

        pixbuf_group = hl_gdk_pixbuf_new("group.png", width=15_c_int, aspect=TRUE)
        pixbuf_input = hl_gdk_pixbuf_new("input.png", width=15_c_int, aspect=TRUE)

        !!!Generate input items
        allocate(item(3))
        !!!Input group 1
!        call group_or_input(item(1)%p, GROUP_TYPE_, [0], name="Input group 1", pixbuf=pixbuf_group)
        item(1)%p => group_or_input(GROUP_TYPE_, [0], name="Input group 1", pixbuf=pixbuf_group)
        !! Input group 1 inputs
        select type(p => item(1)%p)
            type is(group_type)
                allocate(p%item(2))
                !  Text input
!                call group_or_input(p%item(1)%p, INPUT_TYPE_, [0,0], name="input 1.1", &
                p%item(1)%p => group_or_input(INPUT_TYPE_, [0,0], builder, tag=INPUT11_, name="input 1.1", &
                & pixbuf=pixbuf_input, renderer_type=COL_TEXT_INPUT)
                !  Check input
!                call group_or_input(p%item(2)%p, INPUT_TYPE_, [0,1], name="input 1.2", &
                p%item(2)%p => group_or_input(INPUT_TYPE_, [0,1], builder, tag=INPUT12_, name="input 1.2", &
                & pixbuf=pixbuf_input, renderer_type=COL_CHECK_INPUT)
        end select
        !!!Input group 2
!        call group_or_input(item(2)%p, GROUP_TYPE_, [1], name="Input group 2", pixbuf=pixbuf_group)
        item(2)%p => group_or_input(GROUP_TYPE_, [1], name="Input group 2", pixbuf=pixbuf_group)
        !! Input group 2 inputs
        select type(p => item(2)%p)
            type is(group_type)
                allocate(p%item(2))
                !  Radio input
!                call group_or_input(p%item(1)%p, INPUT_TYPE_, [1,0], name="input 2.1", &
                p%item(1)%p => group_or_input(INPUT_TYPE_, [1,0], builder, tag=INPUT21_, name="input 2.1", &
                & pixbuf=pixbuf_input, renderer_type=COL_RADIO_INPUT)
                !  Radio input
!                call group_or_input(p%item(2)%p, INPUT_TYPE_, [1,1], name="input 2.2", &
                p%item(2)%p => group_or_input(INPUT_TYPE_, [1,1], builder, tag=INPUT22_, name="input 2.2", &
                & pixbuf=pixbuf_input, renderer_type=COL_RADIO_INPUT)
        end select
        !!!Input group 3
!        call group_or_input(item(3)%p, GROUP_TYPE_, [2], name="Input group 3", pixbuf=pixbuf_group)
        item(3)%p => group_or_input(GROUP_TYPE_, [2], name="Input group 3", pixbuf=pixbuf_group)
        !! Input group 3 inputs
        select type(p => item(3)%p)
            type is(group_type)
                allocate(p%item(2))
                !  Spin input
!                call group_or_input(p%item(1)%p, INPUT_TYPE_, [2,0], name="input 3.1", &
                p%item(1)%p => group_or_input(INPUT_TYPE_, [2,0], builder, tag=INPUT31_, name="input 3.1", &
                & pixbuf=pixbuf_input, renderer_type=COL_SPIN_INPUT)
                !  Combo input
!                call group_or_input(p%item(2)%p, INPUT_TYPE_, [2,1], name="input 3.2", &
                p%item(2)%p => group_or_input(INPUT_TYPE_, [2,1], builder, tag=INPUT32_, name="input 3.2", &
                & pixbuf=pixbuf_input, renderer_type=COL_COMBO_INPUT, list_combo=["combo1","combo2"])
        end select

        call initialize_inputs(match_tag_with_input_)

        call construct_input_tree(item, treeview_input)

        call gtk_tree_view_expand_all(treeview_input)


        !!!Connect signals with handlers specified in Glade3
        call gtk_builder_connect_signals_full(builder, c_funloc(connect_signals), c_null_ptr)

        call gtk_widget_show_all(window)

        !        call gtk_window_maximize(window)

    end subroutine


end module handlers

program gui_main

    use :: gtk, only: gtk_init, gtk_main
    use :: handlers

    implicit none

    call gtk_init()

    call create_window()

    call show_window()

    call gtk_main()

end program gui_main

