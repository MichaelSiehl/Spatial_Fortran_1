# Approaching General Purpose Data Flow (Spatial) Programming for FPGAs Using Coarray Fortran (as yet only emulated on CPUs)
## Unified Kernel Programming for CPUs and FPGAs Using Coarray Fortran <br /> - Part 1: Defining Some Basics

I open this repository to initially define and describe some basic spatial programming techniques using Coarray Fortran (2018).

*Note: The contents herein are ongoing work and preliminary. Contents may certainly contain mistakes and errors, I do make a number of assumptions and best guesses herein. As yet, all parallel programming here does work efficiently on a CPU and it is only assumed that the same programming will work on FPGAs. **Fortran would then allow to use the same programming on both CPUs and FPGAs.** This work does still describe programming for FPGAs, and even if the Fortran codes are the same for CPUs, describing these codes for programming on CPUs would be very differently.<br />
Some familiarity with Coarray Fortran as well as with spatial programming concepts (as initially described for DPC++) may be required for the reader to be able to follow with the contents here. For links to resources see the section ‘Prerequisites’ at the end.*



## Prologue: A Far-Reaching View for Approaching Data Flow (Spatial) Programming in the Exascale Era

As we are entering the exascale and data flow (spatial) programming era, we should be aware of the extreme changes in the field of programming that we must expect to face in the foreseeable future. Before I start with the contents of this repository, let me briefly describe the big challenges as I see it now: 

- **Using the same programming for CPUs and FPGAs (a topic here):**<br />
Due to the expected very long compile times for FPGAs we must be able to use the same programming on both CPUs and FPGAs, to allow us to develop and test our FPGA codes on a CPU. Otherwise FPGA programming may not be feasible.

- **Transforming procedural programming towards kernel programming (a topic here):**<br />
Kernel programming is the future and the key if we want to profit automatically by any future advances in hardware, no matter if we do programming on CPUs (multicore), on FPGAs, or else. Kernel code has restrictions, and procedures that we may call from such kernel codes must certainly comply to such restrictions as well.

- **Transforming OOP towards Distributed Objects Programming (not a topic here):**<br />
All of my kernel programming is already embedded into distributed objects programming, and I am already using the same OOP syntax (including inheritance) to define and implement distributed objects in Fortran. While the syntax for distributed objects is the same as in OOP, the meaning of the syntax does become something completely different with distributed objects. A crucial advantage of using OOP syntax for Distributed Objects Programming is the already improved compile time analysis for parallel programming in Fortran, since compile time analysis does not distinguish between OOP and Distributed Objects Programming.



## 1.  Introduction

Initially described for (Spatial) DPC++ yet, new generations of FPGAs do already allow new forms of parallel (spatial) programming to deliver unknown levels of parallel performance with a very low energy consumption to hopefully establish a new era of green computing. It is assumed that some LLVM-based Fortran compilers will become spatial compilers in the near future. As yet I do use the traditional Fortran compilers (gfortran/OpenCoarrays and ifort) to emulate spatial programming with Coarray Fortran on CPUs, and do assume (making a number of assumptions) that virtually the same codes will also run on FPGAs with an upcoming spatial compiler.<br />

Coarray Fortran was obviously not specifically designed for, and is not described with respect to spatial programming for data flow architectures. It may therefore be helpful to compare basic programming concepts with (Spatial) DPC++ and to use terminology from (Spatial) DPC++ here.<br />

Coarray Fortran may have an (rather unintended) open design: I’ll utilize coarrays to customize the Fortran language to adapt it to spatial programming techniques.<br />

Coarray Fortran’s design does allow to program basic SPMD coroutines (groups of kernels) on coarray images using (user-defined) traditional blocking synchronization methods. This work will extend this to ***asynchronous coroutines*** to execute multiple different coroutines (still SPMD) simultaneously on each coarray image, using a lightweight and non-blocking synchronization method (through coarray-based, user-defined ***channels***). Asynchronous code execution on each coarray image is the key feature here to allow spatial-like kernels on a CPU and thus, to (hopefully) allow the same programming on both CPUs and FPGAs in the (near) future. <br />

Mapping of such coroutine kernels into spatial pipeline stages and building of spatial pipelines is assumed to go through ***parallel loop instances*** on the coarray images (as well as iterations of each loop instance), as this technique does already work for emulating spatial programming on CPUs here.<br />



## 2.  Crucial Assumptions

This work does rely on a number of assumptions:

#### 1. Execution Segment Ordering (Coarray Fortran)
I do assume that ***execution segment ordering*** with a user-defined ***non-blocking synchronization*** in Coarray Fortran (which does execute Fortran’s SYNC MEMORY statement but is still a different type of image control statement), can be achieved through a ***‘sequentially consistent memory ordering’***. (See sections 3 and 8.1 for details).

#### 2. Mapping of Fortran Kernels through Parallel Loops
I do further assume that mapping of Fortran kernels into spatial pipelines (with future Fortran spatial compilers) will be achieved through *parallel loop instances* on the coarray images (i.e. through the coarray runtime itself), and further through the loop iterations of each loop instance (on each coarray image). (See the sections *‘Parallel Loops in Coarray Fortran’* and *‘Fortran Spatial Kernels’* below).

#### 3. Spatial Coarrays
I do assume that upcoming Fortran spatial compilers will implement coarrays as ***symmetric memory*** (ifort already has) for use with FPGA on-chip memories for efficient FPGA communication between Fortran kernels. (I do further assume that future FPGAs will be enabled for in-memory atomics as I do already massively use atomic coarrays with kernel execution, e.g. to realize a *sequentially consistent memory ordering*, to realize *pairwise independent forward progress* with parallel kernels, and to realize a *fault tolerant execution* with these kernels).

#### 4. Same Coarray Fortran Programming for CPUs and FPGAs
I do also assume that the same Coarray Fortran programming techniques will work very similar on both CPUs and FPGAs. (The programming codes that I am using here do already work efficiently using gfortran/OpenCoarrays and ifort on a CPU).

#### 5. FIFO buffers vs. Asynchronous Coroutines
I am currently reluctant to implement *FIFO buffers* because I can’t see how these will work with asynchronous code execution on each coarray image. Instead I prefer to use *asynchronous coroutines* that do execute simultaneously on each coarray image, so that each coarray image does always execute a portion (kernel) of a task (coroutine). I consider usage of multiple *channels* simultaneously (for communication between kernels of the different *asynchronous coroutines*) as a buffer yet, but can’t tell if this will work as efficiently as *FIFO buffers* on FPGAs. *Asynchronous coroutines* do already work efficiently on a CPU and may qualify for a broaden range of (data flow) algorithms.



## 3.  Ordered Execution Segments with Non-Blocking Synchronization in Coarray Fortran

To allow for asynchronous coroutine execution (where each coarray image will execute multiple coroutines simultaneously), I do implement user-defined, coarray-based *channels* (similar to *channels* with suspending send and receive in Kotlin) that do allow communication between kernels executing on different coarray images, and that do also allow for a lightweight and non-blocking synchronization mechanism (as well as controlling the execution flow) between kernels. (As a counterpart to pipes in DPC++ but without FIFO yet). Such user-defined *channels* do merge (non-atomic) data transfer and (atomic) synchronization into a single process, but can also be used to merely control the (spatial) execution flow atomically.<br />

#### A number of issues may arise from this:

- We can’t use such (user-defined) *non-blocking synchronization* methods to synchronize any data transfer through coarrays, except with those coarrays that are used directly with the synchronization method to implement the (Kotlin-style) channels. As a result, the (lightweight) synchronization and the non-atomic data transfer do form an entity within the user-defined channel. Coarrays are else prohibited with asynchronous code execution because of the else required blocking synchronization with them.

- We can’t use this non-blocking synchronization method alone to ensure the required *execution segment ordering* (Coarray Fortran) with it, instead we must additionally guarantee ‘a *sequentially consistent memory ordering*’ (see DPC++ or ask ChatGPT for a description in simple words, and see also section 8.1 here) by applying a qualified parallel programming model with our programming.

As a result, we are not completely free with the parallel programming styles and models that we are using for asynchronous code execution on each coarray image: We must use the user-defined channel to replace coarrays and we must apply programming styles and models to ensure the now required ‘*sequentially consistent memory ordering*’ with it. By that, **one could (validly) argue that I do break with the rules of Coarray Fortran in favor of parallel programming paradigms that do rather resemble with (Spatial) DPC++**.



## 4.  Parallel Loops in Coarray Fortran

The coarray run-time does inherently allow to use standard loop syntax for implementing parallel loops if the loop does execute on all coarray images (of a coarray team) simultaneously.

Each coarray image does execute it’s own instance of the loop, and loop iterations for each loop instance can be synchronized (usually non-blocking and light-weight) with loop iterations on the other coarray images.


#### *code example 4-1: parallel loop to execute asynchronous coroutines*

```Fortran
parallel_loop: do ! parallel loop does execute on all coarray images simultaneously
  ! calling multiple asynchronous coroutines simultaneously herein
  ! timer for exit the loop:
  call system_clock(count = i_time2); r_timeShift = real(i_time2-i_time1) / r_countRate
  if (r_timeShift > r_timeLimitInSec) exit parallel_loop ! time limit exceeded
end do parallel_loop
```
To emulate spatial kernel programming on a CPU, the execution control of asynchronous coroutines (groups of kernels) shall be placed inside such parallel loops (only one parallel loop per coarray team at the same time). I do assume that future Fortran spatial compilers will utilize such parallel loops to map (groups of) spatial kernels efficiently into spatial pipelines (see the next section). 



## 5.  Fortran Spatial Kernels

For a spatial compiler implementation it should make sense to identify Fortran’s BLOCK construct as an encompassing scoping unit for spatial kernels, as we can already use this syntax for asynchronous kernels on a CPU.


#### *code example 5-1: spatial kernel using a user-defined channel to send data to a different remote kernel on one or several remote coarray images*

```Fortran
kernel_1: block
  integer :: i_value
  real :: r_value
  i_value = 22
  r_value = 22.222
  call channel % fill (i_val = i_value, r_val = r_value)
  call channel % send (i_chstat = i_channelStatus)
  ! this image is ready to execute the next kernel:
  i_channelStatus = enum_channelStatus % kernel_2
end block kernel_1
```
The restrictions for kernel codes are described for DPC++ and do certainly apply to Fortran as well. 

Such Fortran kernels are still very different from DPC++ spatial kernels: The Fortran kernel here does execute through one or multiple instances of a parallel loop, each loop instance of a coarray image will execute it’s own version of the kernel or a different kernel.

The above kernel does use a channel to send data as well as control information (through the i_chstat argument) to a different kernel on one or several remote coarray images.

Thus, in Fortran we (the spatial compiler) can use both, loops and coarrays (through the user-defined *channel*) to **pass data backward to an earlier stage in the spatial pipeline**, in comparison with Spatial DPC++ where they can use intra-kernel pipes only with ND-range kernels and not together with kernels through loops. Also, such Fortran kernels are not simply kernels through loops because the coarray runtime does automatically replicate the kernels for each coarray image. Instead these kernels may even be somewhat similar to ND-range kernels (with the ND-range defined at the level of coarray teams), but with the extension that the compiler might pass (non-coarray) data backward in the pipeline through loop iterations on each coarray image.



## 6. Non-Blocking Synchronization (required for implementing asynchronous coroutines)

Coarray Fortran does inherently allow asynchronous execution of coroutines. Only the required non-blocking synchronization method is not explicitly implemented with the language because it would certainly break with the coarray requirement of ***execution segment ordering***. Instead, we can implement a user-defined non-blocking synchronization method using (atomic) coarrays. This is very much the same as with a blocking synchronization, but with the exception that it does not contain a blocking spin-wait loop with a receive. Simply said, we remove this blocking spin-wait loop away from the synchronization and place it else with the execution control of the encompassing (asynchronous) coroutines.<br />
**To still satisfy the requirement of segment ordering with coarray accesses, we must now apply parallel programming styles and models (with our parallel logic codes else) to ensure the now required ‘*sequentially consistent memory ordering*’ (see DPC++) with it.**

*(Note: In the code examples I am using 'fo %' instead of the usual 'this %' object identifier to make a clear distinction between OOP codes and distributed objects codes. My actual codes are already embedded into a distributed (fragmented) objects model (fo), and I am using the traditional OOP syntax merely to define and implement such distributed objects.)*


#### *code example 6-1: implementing a non-blocking synchronization (send)*

```Fortran
!**** atomic send for synchronization and for control the execution flow: ****
sync memory ! to complete the non-atomic send and to achieve segment ordering
each_image: do i_loopImages = 1, fo % m_i_numberOfRemoteImages
  i_currentRemoteImageNumber = fo % m_ia1_remoteImageNumbers (i_loopImages)
  if (fo % m_l_useRemoteImageNumbersAsArrayIndex) i_arrayIndex = i_currentRemoteImageNumber
  ! set an array element in remote PGAS memory atomically:
  call atomic_define (fo % m_atomic_ia1_channelStatus_cc (i_arrayIndex) [i_currentRemoteImageNumber], i_chStat)
end do each_image
```

#### *code example 6-2: implementing a non-blocking synchronization (receive)*

```Fortran
!**** check the atomic receive for synchronization and to control the execution flow: ****
! non-blocking synchronization to allow for asynchronous coroutines, no spin-wait loop here !
each_image_without_blocking: do i_loopImages = 1, fo % m_i_numberOfRemoteImages
  if (l_useRemoteImageNumbersAsArrayIndex) i_arrayIndex = fo % m_ia1_remoteImageNumbers (i_loopImages)
    ! access an array element in local PGAS memory atomically:
    call atomic_ref (i_channelValue, fo % m_atomic_ia1_channelStatus_cc (i_arrayIndex) [i_thisImage])
    ! check channel status (enum value) to synchronize:
  if (i_chStat == i_channelValue) la1_checkImageChannelStates (i_loopImages) = .true. ! atomic data transfer was successful
end do each_image_without_blocking
!****
if (.not. all(la1_checkImageChannelStates)) then ! the synchronization did not complete (yet)
  isChannelReceive = .false. ! function return value (not successful yet)
  return ! back to the coroutine(s)
end if
!****
! the synchronization did complete:
sync memory ! to complete the non-atomic receive and to achieve segment ordering
isChannelReceive = .true. ! function return value (synchronization was successful)
```

This non-blocking synchronization method is already generic, so that we can execute the same routines on a single image or on several images for send and for receive.

A crucial feature of the Fortran language is the combined usage of array and coarray syntax with the atomic subroutines here, and that this combined usage works with non-atomic coarrays as well: It is the key for collecting data from several coarray images.

Within a coarray team there are usually several instances of these non-blocking synchronizations active at the same time, and they all do execute simultaneously and independently as part of several encompassing asynchronous coroutines within the same (spin-wait) parallel loop.



## 7.  Starting with a Simple Parallel Programming Model

***Single-task kernels*** do execute on a single coarray image and ***multi-task kernels*** (ND-range kernels in DPC++) do execute on multiple coarray images, and these both kernel types do execute simultaneously.<br />

I’ll start using a simple parallel programming model where we always do execute single task kernels (grouped into a *control coroutine*) on coarray image 1 (*control image*) and multi-task kernels (grouped into an *execute coroutine*) on all other coarray images (*execute images*), all within the same coarray team. Both, single-task and multi-task kernels form an entity and are parallel kernels because they can only execute together in parallel with ***pairwise independent forward progress*** (see Spatial DPC++).


#### *code example 7-1: defining a simple parallel programming model*

```Fortran
! default for the control/execute coroutines:
if (this_image() == 1)  then
  ! *** on the control image: ***
  ! defaults for the channel receive with the control coroutine:
  fo % m_i_numberOfRemoteImages = num_images() - 1 ! number of executing images
  fo % m_l_useRemoteImageNumbersAsArrayIndex = .true.
  allocate (fo % m_ia1_remoteImageNumbers (1 : fo % m_i_numberOfRemoteImages))
  do i_loopImages = 1, fo % m_i_numberOfRemoteImages ! the number of execute images
    fo % m_ia1_remoteImageNumbers (i_loopImages) = i_loopImages + 1 ! because image 1 is the control image
  end do
else if (this_image() > 1) then
  ! *** on the execute images: ***
  ! defaults for the channel receive with the execute coroutines:
  fo % m_i_numberOfRemoteImages = 1
  fo % m_l_useRemoteImageNumbersAsArrayIndex = .false.
  allocate (fo % m_ia1_remoteImageNumbers (1 : fo % m_i_numberOfRemoteImages))
  fo % m_ia1_remoteImageNumbers (1) = 1 ! the control image number
end if
```

I consider this simple parallel programming model already as a foundation for achieving the required ‘***sequentially consistent memory ordering***’.



## 8.  Grouping Kernels into Coroutines and Combining Kernels from Different Coroutines into Fortran Submodules

A coroutine is a convenient way to group multiple kernels together. Here we start with the simple parallel programming model where we group all single-task kernels into a *control coroutine* and where we group all multi-task kernels into an *execute coroutine*.

We usually combine the kernels from our both coroutines (*control* and *execute coroutines*) into a single Fortran submodule, so that their codes form an integrated entity. This is not required for compiling and executing the codes, but it is a very convenient way to write parallel algorithms because we can bring together the codes as they do form an algorithm.


#### *code example 8-1: combining kernels from different coroutines*

```Fortran
subtask1_select: select case (i_imageType)
!========================================================
! control coroutine:
case (enum_imageType % controlImage) ! on the control image
  control_coroutine_subtask1: block
    i_testValue = 22
    r_testValue = 2.222
    ia1_testArray = (/22,222,22222/)
    call channel2 % fill (i_val = i_testValue, r_val = r_testValue, &
                               ia1_val = ia1_testArray)
    call channel2 % send (i_chstat = i_channel2Status)
    ! this image is ready for the next task:
    i_channel2Status = enum_channel2Status_CE1 % subtask2
  end block control_coroutine_subtask1
!========================================================
! execute coroutine:
case (enum_imageType % executeImage) ! on the execute images
  execute_coroutine_subtask1: block
    integer, dimension (1:1) :: ia1_scalarInteger
    real, dimension (1:1) :: ra1_scalarReal
    integer, dimension(1:3, 1:1) :: ia2_integerArray1D
    ! always use only a single channel within a block with IsReceive !
    ! (otherwise the data transfer through a channel will not synchronize successfully) !
    if (channel2 % isReceive (i_chstat = i_channel2Status)) then
      call channel2 % get (ia1_scalarInteger = ia1_scalarInteger, &
                                ra1_scalarReal = ra1_scalarReal, &
                                ia2_integer1D = ia2_integerArray1D)
      i_testValue = ia1_scalarInteger (1)
      r_testValue = ra1_scalarReal (1)
      ia1_testArray = ia2_integerArray1D (:,1)
      ! isReceive was successful, this image is ready for the next task:
      i_channel2Status = enum_channel2Status_CE1 % subtask2
      call system_clock(count = i_Time1) ! reset the timer
    end if
  end block execute_coroutine_subtask1
!========================================================
! error: unclassified image
case default
  return
end select subtask1_select
```

The code example shows two kernels from the different coroutines (*control* and *execute*) combined together in code but executing on distinct coarray images (spatial pipelines) with pairwise forward progress.

A channel is used for inexpensive communication between the kernels. Inexpensive also because the kernel of the execute coroutine does not execute before the synchronization (and data transfer) through the channel has completed, and in the meantime the coarray image (spatial pipeline) will execute another kernel from another asynchronous coroutine instead.

A spatial compiler should be able to handle the control flow (select case) as a means to map the kernels efficiently to spatial areas on FPGAs.

The next code example shows how programming at the SUBMODULE level is like, when we combine the two coroutines into a single MODULE PROCEDURE.


#### *code example 8-2: combining two coroutines into a single MODULE PROCEDURE*

```Fortran
submodule (frob03_01_cls_FM3_CE1_SPMD) frob03_01_sub_FM3_CE1_SM2
! using Channel2 for the control and execute coroutines herein,
! and Channel4 for send to another coroutine in submodule frob03_01_sub_FM3_CE1_SM3
implicit none

contains

module procedure frob03_01_FM3_CE1_SM2

!----------------------------------------------------------------------------------------
! (1) subtask1 block:
subtask1_if: if (i_channel2Status == enum_channel2Status_CE1 % subtask1) then
subtask1_block: block
  integer :: i_testValue
  real :: r_testValue
  integer, dimension(1:3) :: ia1_testArray

  subtask1_select: select case (i_imageType)
  !========================================================

  ! control coroutine:
  case (enum_imageType % controlImage) ! on the control image
    control_coroutine_subtask1: block
      i_testValue = 22
      r_testValue = 2.222
      ia1_testArray = (/22,222,22222/)
      call channel2 % fill (i_val = i_testValue, r_val = r_testValue, &
                                 ia1_val = ia1_testArray)
      call channel2 % send (i_chstat = i_channel2Status)
      ! this image is ready for the next subtask:
      i_channel2Status = enum_channel2Status_CE1 % subtask2
      !*** sending to frob03_01_FM3_CE1_SM3 using Channel4:
      ! (we can use multiple channels in the same block for sending)
      if (i_channel4Status == enum_channel4Status_CE1 % subtask1) then
        r_testValue = 4.444
        call channel4 % fill (r_val = r_testValue)
        call channel4 % send (i_chstat = i_channel4Status)
        ! this image is ready for the next subtask for Channel4 (in this case e_xit):
        i_channel4Status = enum_channel4Status_CE1 % e_xit
      end if
    end block control_coroutine_subtask1
  !========================================================

  ! execute coroutine:
  case (enum_imageType % executeImage) ! on the execute images
    execute_coroutine_subtask1: block
      integer, dimension (1:1) :: ia1_scalarInteger
      real, dimension (1:1) :: ra1_scalarReal
      integer, dimension(1:3, 1:1) :: ia2_integerArray1D
      ! always use only a single channel within a block with IsReceive !
      ! (otherwise the data transfer through a channel will not synchronize successfully) !
      if (channel2 % IsReceive (i_chstat = i_channel2Status)) then
        call channel2 % get (ia1_scalarInteger = ia1_scalarInteger, &
                                  ra1_scalarReal = ra1_scalarReal, &
                                  ia2_integer1D = ia2_integerArray1D)
        i_testValue = ia1_scalarInteger (1)
        r_testValue = ra1_scalarReal (1)
        ia1_testArray = ia2_integerArray1D (:,1)
        write(*,*) 'from channel2:', i_testValue ! usually not supported in kernels
        write(*,*) 'from channel2: ', r_testValue
        write(*,*) 'from channel2: ', ia1_testArray
        ! IsReceive was successful, this image is ready for the next subtask:
        i_channel2Status = enum_channel2Status_CE1 % subtask2
        call system_clock(count = i_time1) ! reset the timer
      end if
    end block execute_coroutine_subtask1
  !========================================================

  ! error: unclassified image
  case default
    return
  end select subtask1_select
end block subtask1_block
end if subtask1_if
! (1) end subtask1 block
!----------------------------------------------------------------------------------------


!----------------------------------------------------------------------------------------
! (2) subtask2 block:
subtask2_if: if (i_channel2Status == &
                 enum_channel2Status_CE1 % subtask2) then
subtask2_block: block

  subtask2_select: select case (i_imageType)
  !========================================================

  ! execute coroutine:
  case (enum_imageType % executeImage) ! on the execute images
    execute_coroutine_subtask2: block
      integer :: i_testValue
      real :: r_testValue
      integer, dimension(1:3) :: ia1_testArray
      i_testValue = 222222
      r_testValue = 22222.222
      ia1_testArray = (/22222,2222222,22222222/)
      call channel2 % fill (i_val = i_testValue, r_val = r_testValue, &
                                                    ia1_val = ia1_testArray)
      call channel2 % send (i_chstat = i_channel2Status)
      ! this image is ready for the next task:
      i_channel2Status = enum_channel2Status_CE1 % e_xit
    end block execute_coroutine_subtask2
  !========================================================

  ! control coroutine:
  case (enum_imageType % controlImage) ! on the control image
    control_coroutine_subtask2: block
      integer, dimension (1:i_numberOfExecutingImages) :: ia1_scalarInteger
      real, dimension (1:i_numberOfExecutingImages) :: ra1_scalarReal
      integer, dimension(1:3, 1:i_numberOfExecutingImages) :: ia2_integerArray1D
      ! always use only a single channel within a block with IsReceive !
      ! (otherwise the data transfer through a channel will not synchronize successfully) !
      if (channel2 % IsReceive (i_chstat = i_channel2Status)) then
        call channel2 % get (ia1_scalarInteger = ia1_scalarInteger, &
                                  ra1_scalarReal = ra1_scalarReal, &
                                  ia2_integer1D = ia2_integerArray1D)
        write(*,*) 'from channel2: ', ia1_scalarInteger(:) ! usually not supported in kernels
        write(*,*) 'from channel2: ', ra1_scalarReal(:)
        write(*,*) 'from channel2: ', ia2_integerArray1D(:,:)
        ! IsReceive was successful, this image is ready for the next task:
        i_channel2Status = enum_channel2Status_CE1 % e_xit
        call system_clock(count = i_time1) ! reset the timer
      end if
    end block control_coroutine_subtask2
  !========================================================
  
  ! error: unclassified image
  case default
    return
  end select subtask2_select

end block subtask2_block
end if subtask2_if
! (2) end subtask2 block
!----------------------------------------------------------------------------------------

end procedure frob03_01_FM3_CE1_SM2

end submodule frob03_01_sub_FM3_CE1_SM2
```

The module procedure in code example 8-2 contains 4 kernels from 2 coroutines. Each of the coroutines comprises two kernels that do execute on the same coarray image(s) respectively: The kernels of the control coroutine do execute on a single control image (single-task kernels), and the kernels of the execute coroutine do execute on the multiple execute images (multi-task kernels).

The following subsections will refer to this code example to explain the underlying programming techniques.



### 8.1  Sequentially Consistent Memory Ordering

Each (successful) kernel execution involves an atomic (in-memory) operation through the channel’s *i_chstat* argument, either through a send or through a receive (see code examples 6-1 and 6-2). This leads to an ***acquire-release* memory ordering**, which is already one key for establishing the ***sequentially consistent* memory ordering**.

Additionaly, I am using an integer-based enumeration technique to assign values in the coroutines. These assigned integer-based enum values are another key to ensure the required ***sequentially consistent* memory ordering** with the execution of the coroutines here, because atomic operations are ordered yet.



### 8.2  Fault Tolerant Execution with Pairwise Independent Forward Progress

The execution sequence of the kernels in a coroutine are sequentially ordered through the channel’s *i_chstat* argument, even if the ordering may change during execution. A crucial property is a constant swap of channel send and receive for successive execution of kernels on the coarray images. This is to ensure pairwise (independent) forward progress for parallel kernel execution.

The execution control of the coroutines uses the atomic operation behind this send and receive. And, if something goes wrong within kernel execution, execution control uses a missing atomic operation as a signal for a potential failure. (Coarray images can then remain in a short ‘wait mode’ to see if the ‘failed’ coarray image was only delayed and not really a failure).

Strategies for handling such failures could be to recover by leaving and reentering a coarray team, or to simply continue execution with other channel instances (and other asynchronous coroutines) if this is still possible. (We may check for that possibility in the encompassing parallel loop).



### 8.3  Asynchronous Code Execution on the Single Coarray Images

As the channel’s *IsReceive* function is non-blocking, we can use the encompassing parallel loop to implement an execution control that simultaneously executes multiple such MODULE PROCEDURES inside that parallel loop. A spatial compiler should be enabled to generate multiple (or merge into) spatial pipelines from the control flow (if-else / select-case). 

Whereas on a CPU: Only one kernel per coarray image get’s executed at a time with loop iterations, but we can still achieve more kernel execution across iterations as we have more (asynchronous) kernels available in a queue on each coarray image. To me these multiple asynchronous kernels on each coarray image are the key that should allow us to use the same programming on CPUs and FPGAs here. But on FPGAs, I would expect higher performance if kernels (the workload of each coarray image) are distributed to multiple spatial pipelines that can execute the workload of a single coarray image in parallel.



### 8.4  Collecting Data Across Spatial Pipelines

Under the assumption that a single coarray image will lead into one spatial pipeline (and also if it’s more), there will be one spatial pipeline for a single-task coroutine (kernels) and multiple spatial pipelines for a multi-task coroutine (kernels).

The last kernel in *code example 8-2* does call the channel’s *IsReceive* function on the single-task (*control*) image. That call does already collect data from the multiple sends of the multi-task (*execute*) images. That is also why we have to use arrays there, for the receive of the multiple scalar values.

This property of our parallel programming model here together with the user-defined channel does provide great flexibility on a CPU already to collect distributed data regularly, and could be another foundation for a wide range of data flow algorithms on FPGAs.



## 9.  Prerequisites

I’d recommend the following links to resources to get started:

#### Spatial Accelerator Architectures:
- Intel’s Exascale Dataflow Engine Drops X86 And Von Neumann: https://www.nextplatform.com/2018/08/30/intels-exascale-dataflow-engine-drops-x86-and-von-neuman/
- Evaluating Spatial Accelerator Architectures with Tiled Matrix-Matrix Multiplication: https://arxiv.org/pdf/2106.10499.pdf

#### Coarray Fortran:
- Coarrays in the next Fortran Standard (pdf): https://wg5-fortran.org/N1801-N1850/N1824.pdf
- Additional Parallel Features in Fortran (pdf): https://wg5-fortran.org/N2001-N2050/N2007.pdf
- Additional Parallel Features in Fortran (pdf, slices): https://www.lrz.de/services/compute/courses/x_lecturenotes/Parallel_Programming_Languages_Workshop/Coarray-Fortran.pdf
- The Asynchronous Partitioned Global Address Space Model: https://www.cs.rochester.edu/u/cding/amp/papers/full/The%20Asynchronous%20Partitioned%20Global%20Address%20Space%20Model.pdf
- Rationale for Co-Arrays in Fortran 2008: http://caf.rice.edu/documentation/Aleksandar-Donev-Coarray-Rationale-N1702-2007-09.pdf 
- Fanfarillo, A., Burnus, T., Cardellini, V., Filippone, S., Nagle, D., & Rouson, D. (2014, October). OpenCoarrays: open-source transport layers supporting coarray Fortran compilers. In Proceedings of the 8th International Conference on Partitioned Global Address Space Programming Models (p. 4). ACM.: http://www.opencoarrays.org/

#### Spatial (FPGA) Programming using DPC++:
- Data Parallel C++, Mastering DPC++ for Programming of Heterogeneous Systems using C++ and SYCL (book as pdf freely available): https://link.springer.com/book/10.1007/978-1-4842-5574-2
- Spatial DPC++ Constructs for Algorithm Acceleration with FPGAs (video): https://www.intel.com/content/www/us/en/developer/videos/spatial-dpc-constructs-algorithm-acceleration-fpga.html
https://www.oneapi.io/event-sessions/spatial-dpc-constructs-for-algorithm-acceleration-with-fpgas/

#### Coroutine Tutorial for C#:
- What is a Coroutine? : https://www.codeproject.com/Tips/5262735/What-is-a-Coroutine

#### Channels in Kotlin:
https://kotlinlang.org/docs/channels.html 

