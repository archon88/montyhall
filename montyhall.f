      program montyhall
      implicit none
            
      integer ctr/0/
      integer i_rand/0/
      integer no_iterations/0/
      integer*4 timeArray(3)
      integer door_with_car/0/
      integer door_chosen/0/
      integer door_to_open/0/
      integer keeper_success/0/
      integer changer_success/0/      

      open(unit = 17, file = "change.dat")
      open(unit = 18, file = "keep.dat")

      print *,'Enter number of iterations'
      read *, no_iterations
      
      call itime(timeArray)

      i_rand=rand(timeArray(1)+timeArray(2)+timeArray(3))                            
      
      
      do ctr=1, no_iterations
         
         door_with_car=int(rand(0)*3)+1

         door_chosen = int(rand(0)*3)+1
         
         door_to_open = int(rand(0)*3)+1

         do while (door_to_open.eq.door_chosen
     +        .or.door_to_open.eq.door_with_car)

            door_to_open=int(rand(0)*3)+1

         end do
         
         
C     For changer

         write(17,*) 'Door originally chosen was ', door_chosen
         write(17,*) 'Door opened by MH was ', door_to_open
         write(17,*) 'Door with car was ', door_with_car

         if(door_chosen.ne.door_with_car) then
            
            write(17,*) 'SUCCESS'
            write(17,*) ' '
            write(17,*) ' '
            
            changer_success=changer_success+1
            
         else
            
            write(17,*) 'FAILURE'
            write(17,*) ' '
            write(17,*) ' '
            
         endif


         
C     For keeper
         
         write(18,*) 'Door originally chosen was ', door_chosen
         write(18,*) 'Door opened by MH was ', door_to_open
         write(18,*) 'Door with car was ', door_with_car
         
         if(door_chosen.eq.door_with_car) then
            
            write(18,*) 'SUCCESS'
            write(18,*) ' '
            write(18,*) ' '
            
            keeper_success=keeper_success+1
            
         else
            
            write(18,*) 'FAILURE'
            write(18,*) ' '
            write(18,*) ' '
            
         endif
         
      end do
      
      write(17,*) 'Out of ',no_iterations,'tries, changer succeeded ', 
     +     changer_success, ' times.', 'This corresponds to a success ',
     +     'rate of ', 100*changer_success/no_iterations,'%.'
      
      write(18,*) 'Out of ',no_iterations,'tries, keeper succeeded ', 
     +     keeper_success, ' times.', 'This corresponds to a success ',
     +     'rate of ', 100*keeper_success/no_iterations,'%.'
  
      close(17)
      close(18)
      
      print*,'The success rates of the change/keep strategies were ',   &
     &      100*changer_success/no_iterations,'% and ', 
     &      100*keeper_success/no_iterations,'% respectively.'

      return
      end
