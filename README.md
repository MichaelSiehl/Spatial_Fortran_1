# General Purpose Data Flow (Spatial) Programming for FPGAs Using Coarray Fortran (as yet only emulated on CPUs)
## (Unified Kernel Programming for CPUs and FPGAs Using Coarray Fortran)
## Part 1: Defining Some Basics

I open this repository to initially define and describe some basic spatial programming techniques using Coarray Fortran (2018).

*Note: The contents herein are ongoing work and preliminary. Contents may certainly contain mistakes and errors, I do make a number of assumptions and best guesses herein. As yet, all parallel programming here does work efficiently on a CPU and it is only assumed that the same programming will work on FPGAs. **Fortran would then allow to use the same programming on both CPUs and FPGAs.** This work does still describe programming for FPGAs, and even if the Fortran codes are the same for CPUs, describing these codes for programming on CPUs would be very differently.<br />
Some familiarity with Coarray Fortran as well as with spatial programming concepts (as initially described for DPC++) may be required for the reader to be able to follow with the contents here. For links to resources see the section ‘Prerequisites’ at the end.*



## 1.  Prolog

Initially described for (Spatial) DPC++ yet, new generations of FPGAs do already allow new forms of parallel (spatial) programming to deliver unknown levels of parallel performance with a very low energy consumption to hopefully establish a new era of green computing. It is assumed that some LLVM-based Fortran compilers will become spatial compilers in the near future. As yet I do use the traditional Fortran compilers (gfortran/OpenCoarrays and ifort) to emulate spatial programming with Coarray Fortran on CPUs, and do assume (making a number of assumptions) that virtually the same codes will also run on FPGAs with an upcoming spatial compiler.<br />

Coarray Fortran was obviously not specifically designed for, and is not described with respect to spatial programming for data flow architectures. It may therefore be helpful to compare basic programming concepts with (Spatial) DPC++ and to use terminology from (Spatial) DPC++ here.<br />

Coarray Fortran may have an (rather unintended) open design: I’ll utilize coarrays to customize the Fortran language to adapt it to spatial programming techniques.<br />

Coarray Fortran’s design does allow to program basic SPMD coroutines (groups of kernels) on coarray images using (user-defined) traditional blocking synchronization methods. This work will extend this to ***asynchronous coroutines*** to execute multiple different coroutines (still SPMD) simultaneously on each coarray image, using a lightweight and non-blocking synchronization method (through coarray-based, user-defined ***channels***). Asynchronous code execution on each coarray image is the key feature here to allow spatial-like kernels on a CPU and thus, to (hopefully) allow the same programming on both CPUs and FPGAs in the (near) future. <br />

Mapping of such coroutine kernels into spatial pipeline stages and building of spatial pipelines is assumed to go through ***parallel loop instances*** on the coarray images (as well as iterations of each loop instance), as this technique does already work for emulating spatial programming on CPUs here.<br />



## 2.  Crucial Assumptions

This work does rely on a number of assumptions:

#### 1. Execution Segment Ordering (Coarray Fortran)
I do assume that ***execution segment ordering*** with a user-defined ***non-blocking synchronization*** in Coarray Fortran (which does execute Fortran’s SYNC MEMORY statement but is still a different type of image control statement), can be achieved through a ***‘sequential consistent memory ordering’***. (See the next section for details).

#### 2. Mapping of Fortran Kernels through Parallel Loops
I do further assume that mapping of Fortran kernels into spatial pipelines (with future Fortran spatial compilers) will be achieved through *parallel loop instances* on the coarray images (i.e. through the coarray runtime itself), and further through the loop iterations of each loop instance (on each coarray image). (See the sections *‘Parallel Loops in Coarray Fortran’* and *‘Fortran Spatial Kernels’* below).

#### 3. Spatial Coarrays
I do assume that upcoming Fortran spatial compilers will implement coarrays as ***symmetric memory*** (ifort already has) for use with FPGA on-chip memories for efficient FPGA communication between Fortran kernels.

#### 4. Same Coarray Fortran Programming for CPUs and FPGAs
I do also assume that the same Coarray Fortran programming techniques will work very similar on both CPUs and FPGAs. (The programming codes that I am using here do already work efficiently using gfortran/OpenCoarrays and ifort on a CPU).

#### 5. FIFO buffers vs. Asynchronous Coroutines
I am currently reluctant to implement *FIFO buffers* because I can’t see how these will work with asynchronous code execution on each coarray image. Instead I prefer to use *asynchronous coroutines* that do execute simultaneously on each coarray image, so that each coarray image does always execute a portion (kernel) of a task (coroutine). I consider usage of multiple *channels* simultaneously (for communication between kernels of the different *asynchronous coroutines*) as a buffer yet, but can’t tell if this will work as efficiently as *FIFO buffers* on FPGAs. *Asynchronous coroutines* do already work efficiently on a CPU and may qualify for a broaden range of (data flow) algorithms.



## 3.  Ordered Execution Segments with Non-Blocking Synchronization in Coarray Fortran

To allow for asynchronous coroutine execution (where each coarray image will execute multiple coroutines simultaneously), I do implement user-defined, coarray-based *channels* (similar to *channels* with suspending send and receive in Kotlin) that do allow communication between kernels executing on different coarray images, and that do also allow for a lightweight and non-blocking synchronization mechanism (as well as controlling the execution flow) between kernels. (As a counterpart to pipes in DPC++ but without FIFO yet). Such user-defined *channels* do merge (non-atomic) data transfer and (atomic) synchronization into a single process, but can also be used to merely control the (spatial) execution flow atomically.<br />

#### A number of issues may arise from this:

- We can’t use such (user-defined) *non-blocking synchronization* methods to synchronize any data transfer through coarrays, except with those coarrays that are used directly with the synchronization method to implement the (Kotlin-style) channels. As a result, the (lightweight) synchronization and the non-atomic data transfer do form an entity within the user-defined channel. Coarrays are else prohibited with asynchronous code execution because of the else required blocking synchronization with them.

- We can’t use this non-blocking synchronization method alone to ensure the required *execution segment ordering* (Coarray Fortran) with it, instead we must additionally guarantee ‘a *sequentially consistent memory ordering*’ (see DPC++ or ask OpenGPT for a description in simple words) by applying a qualified parallel programming model with our programming.

As a result, we are not completely free with the parallel programming styles and models that we are using for asynchronous code execution on each coarray image: We must use the user-defined channel to replace coarrays and we must apply programming styles and models to ensure the now required ‘*sequentially consistent memory ordering*’ with it. By that, **one could (validly) argue that I do break with the rules of Coarray Fortran in favor of parallel programming paradigms that do rather resemble with (Spatial) DPC++**.



## 4.  Parallel Loops in Coarray Fortran

The coarray run-time does inherently allow to use standard loop syntax for implementing parallel loops if the loop does execute on all coarray images (of a coarray team) simultaneously.

Each coarray image does execute it’s own instance of the loop, and loop iterations for each loop instance can be synchronized (usually non-blocking and light-weight) with loop iterations on the other coarray images.


#### *code example: parallel loop to execute asynchronous coroutines*

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


#### *code example: spatial kernel using a user-defined channel to send data to a different remote kernel on one or several remote coarray images*

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


#### *code example: implementing a non-blocking synchronization (send)*

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

#### *code example: implementing a non-blocking synchronization (receive)*

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


#### *code example: defining a simple parallel programming model*

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

I consider this simple parallel programming model already as a foundation to achieve the required ‘***sequentially consistent memory ordering***’.



## 8.  Grouping Kernels into Coroutines (and combining kernels from multiple coroutines into Fortran submodules)

A coroutine is a convenient way to group multiple kernels together. Here we start with the simple parallel programming model where we group all single-task kernels into a *control coroutine* and where we group all multi-task kernels into an *execute coroutine*.

We usually combine the kernels from our both coroutines (*control* and *execute coroutines*) into a single Fortran submodule, so that their codes form an integrated entity. This is not required for compiling and executing the codes, but it is a very convenient way to write parallel algorithms because we can bring together the codes that form an algorithm, which will not match with the actual parallel code execution sequence (on any hardware).


#### *code example: grouping kernels from different coroutines*

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

#### Spatial (FPGA) Programming using DPC++:
- Data Parallel C++, Mastering DPC++ for Programming of Heterogeneous Systems using C++ and SYCL (book as pdf freely available): https://link.springer.com/book/10.1007/978-1-4842-5574-2
- Spatial DPC++ Constructs for Algorithm Acceleration with FPGAs (video): https://www.intel.com/content/www/us/en/developer/videos/spatial-dpc-constructs-algorithm-acceleration-fpga.html
https://www.oneapi.io/event-sessions/spatial-dpc-constructs-for-algorithm-acceleration-with-fpgas/

#### Coroutine Tutorial for C#:
- What is a Coroutine? : https://www.codeproject.com/Tips/5262735/What-is-a-Coroutine

#### Channels in Kotlin:
https://kotlinlang.org/docs/channels.html 

