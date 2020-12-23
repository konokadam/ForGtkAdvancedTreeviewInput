module tag_input_match

    implicit none
    private
    interface assignment(=)
        module procedure :: get_real_input, get_integer_input, &
        & get_character_input, get_logical_input
    end interface

    public :: assignment(=)

contains

    subroutine get_real_input(param, input)

        class(*), intent(in) :: input
        real, intent(out) :: param

        integer :: iostat

        select type(input)
            type is(real)
                param = input
            type is(character(*))
                read(input,*,iostat=iostat) param
                !if(iostat == 0) then
                !    print*, param
                !else
                !    print*, "Wrong input "!for "//trim(tag)//" which should be real: "//trim(input)
                !endif
        end select

    end subroutine

    subroutine get_integer_input(param, input)

        class(*), intent(in) :: input
        integer, intent(out) :: param

        integer :: iostat

        select type(input)
            type is(integer)
                param = input
            type is(character(*))
                read(input,*,iostat=iostat) param
                !if(iostat == 0) then
                !    print*, param
                !else
                !    print*, "Wrong input "!for "//trim(tag)//" which should be real: "//trim(input)
                !endif
        end select

    end subroutine

    subroutine get_character_input(param, input)

        class(*), intent(in) :: input
        character(*), intent(out) :: param

        select type(input)
            type is(character(*))
                param = trim(input)
                !print*, trim(param)
        end select

    end subroutine

    subroutine get_logical_input(param, input)

        class(*), intent(in) :: input
        logical, intent(out) :: param

        select type(input)
            type is(logical)
                param = input
                !print*, param
        end select

    end subroutine

end module

