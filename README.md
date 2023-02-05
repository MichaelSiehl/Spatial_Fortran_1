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

Coarray Fortran’s design does allow to program basic SPMD coroutines (groups of kernels) on coarray images using (user-defined) traditional blocking synchronization methods. This work will extend this to ***asynchronous coroutines*** to execute multiple different coroutines (still SPMD) simultaneously on each coarray image, using a lightweight and non-blocking synchronization method (through coarray-based, user-defined ***channels***).<br />

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


#### *Code example: parallel loop to execute asynchronous coroutines*

```Fortran
parallel_loop: do ! parallel loop does execute on all coarray images simultaneously
  ! calling multiple asynchronous coroutines simultaneously herein
  ! timer for exit the loop:
  call system_clock(count = i_time2); r_timeShift = real(i_time2-i_time1) / r_countRate
  if (r_timeShift > r_timeLimitInSec) exit parallel_loop ! time limit exceeded
end do parallel_loop
```
To emulate spatial kernel programming on a CPU, the execution control of asynchronous coroutines (groups of kernels) shall be placed inside such parallel loops (only one parallel loop per coarray team at the same time). I do assume that future Fortran spatial compilers will utilize such parallel loops to map (groups of) spatial kernels efficiently into spatial pipelines (see the next section). 

