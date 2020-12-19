module params

    implicit none
    real :: input11 = 0.
    logical :: input12 = .false.
    character(20) :: input2 = ""
    integer :: input31 = 0
    character(20) :: input32 = ""

    character(20) :: input2s(2)

    character(10), parameter :: INPUT11_ ="input11"
    character(10), parameter :: INPUT12_ ="input12"
    character(10), parameter :: INPUT21_ ="input21"
    character(10), parameter :: INPUT22_ ="input22"
    character(10), parameter :: INPUT31_ ="input31"
    character(10), parameter :: INPUT32_ ="input32"

contains

    !!!Procedure to get input from treeview by tag
    subroutine match_tag_with_input_(tag, input)

        character(*), intent(in) :: tag
        class(*), intent(in) :: input

        integer :: iostat

        print*,"    "
        print*, trim(tag)//" edited"

        input2s(1) = "input21_"
        input2s(2) = "input22_"

        select case(tag)
            case(INPUT11_)
                select type(input)
                    type is(real)
                        input11 = input
                    type is(character(*))
                        read(input,*,iostat=iostat) input11
                        if(iostat == 0) then
                            print*, input11
                        else
                            print*, "Wrong input for "//trim(tag)//" which should be real: "//trim(input)
                        endif
                end select
            case(INPUT12_)
                select type(input)
                    type is(logical)
                        input12 = input
                        print*, input12
                end select
            case(INPUT21_)
                select type(input)
                    type is(logical)
                        if(input) input2 = input2s(1)
                        if(input) print*, "input21"//" selected to give: "//trim(input2)
                end select
            case(INPUT22_)
                select type(input)
                    type is(logical)
                        if(input) input2 = input2s(2)
                        if(input) print*, "input22"//" selected to give: "//trim(input2)
                end select
            case(INPUT31_)
                select type(input)
                    type is(integer)
                        input31 = input
                   type is(character(*))
                        read(input,*,iostat=iostat) input31
                        if(iostat == 0) then
                            print*, input31
                        else
                            print*, "Wrong input for "//trim(tag)//" which should be real: "//trim(input)
                        endif
                end select
            case(INPUT32_)
                select type(input)
                    type is(character(*))
                        input32 = trim(input)
                        print*, trim(input32)
                end select
            case default
                print*, "Wrong tag has been sent: "//trim(tag)
        end select

    end subroutine

end module
