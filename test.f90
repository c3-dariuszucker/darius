program battleship_test
  implicit none

  ! Board size
  integer, parameter :: N = 10

  ! Types
  character(len=1), dimension(N, N) :: board
  logical :: success
  character :: result
  integer :: i

  interface
    subroutine init_board(board)
      character(len=1), dimension(:,:) :: board
    end subroutine

    subroutine place_ship(board, row, col, size, direction, success)
      character(len=1), dimension(:,:) :: board
      integer :: row, col, size
      character(len=1) :: direction
      logical :: success
    end subroutine

    subroutine attack(board, row, col, result)
      character(len=1), dimension(:,:) :: board
      integer :: row, col
      character :: result
    end subroutine

    logical function all_ships_sunk(board)
      character(len=1), dimension(:,:) :: board
    end function
  end interface
