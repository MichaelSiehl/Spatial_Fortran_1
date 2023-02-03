# General Purpose Data Flow (Spatial) Programming for FPGAs Using Coarray Fortran (as yet only emulated on CPUs): 1. Defining Some Basics

I open this repository to initially define and describe some basic spatial programming techniques using Coarray Fortran (2018).

*Note: The contents herein are ongoing work and preliminary. Contents may certainly contain mistakes and errors, I do make a number of assumptions and best guesses herein. As yet, all parallel programming here does work efficiently on a CPU and it is only assumed that the same programming will work on FPGAs. **Fortran would then allow to use the same programming on both CPUs and FPGAs.** This work does still describe programming for FPGAs, and even if the Fortran codes are the same for CPUs, describing these codes for programming on CPUs would be very differently.<br />
Some familiarity with Coarray Fortran as well as with spatial programming concepts (as initially described for DPC++) may be required for the reader to be able to follow with the contents here. For links to resources see the section ‘Prerequisites’ at the end.*



## 1.  Prolog

Initially described for (Spatial) DPC++ yet, new generations of FPGAs do already allow new forms of parallel (spatial) programming to deliver unknown levels of parallel performance with a very low energy consumption to hopefully establish a new era of green computing. It is assumed that some LLVM-based Fortran compilers will become spatial compilers in the near future. As yet I do use the traditional Fortran compilers (gfortran/OpenCoarrays and ifort) to emulate spatial programming with Coarray Fortran on CPUs, and do assume (making a number of assumptions) that virtually the same codes will also run on FPGAs with an upcoming spatial compiler.<br />

Coarray Fortran was obviously not specifically designed for, and is not described with respect to spatial programming for data flow architectures. It may therefore be helpful to compare basic programming concepts with (Spatial) DPC++ and to use terminology from (Spatial) DPC++ here.<br />

Coarray Fortran may have an (rather unintended) open design: I’ll utilize coarrays to customize the Fortran language to adapt it to spatial programming techniques that are initially described for (Spatial) DPC++ yet.<br />

Coarray Fortran’s design does allow to program basic SPMD coroutines (groups of kernels) on coarray images using (user-defined) traditional blocking synchronization methods. This work will extend this to ***asynchronous coroutines*** to execute multiple different coroutines (still SPMD) simultaneously on each coarray image, using a lightweight and non-blocking synchronization method (through coarray-based user-defined ***channels***).<br />

Mapping of such coroutine kernels into spatial pipeline stages and building of spatial pipelines is assumed to go through ***parallel loop instances*** on the coarray images (as well as iterations of each loop instance), as this technique does already work for emulating spatial programming on CPUs here.<br />



## 2.  Crucial Assumptions

This work does rely on a number of assumptions:

#### 1. Execution Segment Ordering (Coarray Fortran)
I do assume that ***execution segment ordering*** with a user-defined ***non-blocking synchronization*** in Coarray Fortran (which does execute Fortran’s SYNC MEMORY statement but is still a different type of image control statement), can be achieved through a ***‘sequential consistent memory ordering’***. (See the next section for details).

#### 2. Mapping of Fortran Kernels through Parallel Loops
I do further assume that mapping of Fortran kernels into spatial pipelines (with future Fortran spatial compilers) will be achieved through *parallel loop instances* on the coarray images (i.e. through the coarray runtime itself), and further through the loop iterations of each loop instance (on each coarray image). (See the sections *‘Parallel Loops in Coarray Fortran’* and *‘Fortran Spatial Kernels’* below).

#### 3. Spatial Coarrays
I do assume that upcoming Fortran spatial compilers will implement coarrays as symmetric memory (ifort already has) for use with FPGA on-chip memories for efficient FPGA communication between Fortran kernels.

#### 4. Same Coarray Fortran Programming for CPUs and FPGAs
I do also assume that the same Coarray Fortran programming techniques will work very similar on both CPUs and FPGAs. (The programming codes that I am using here do already work efficiently using gfortran/OpenCoarrays and ifort on a CPU).

#### 5. FIFO buffers vs. Asynchronous Coroutines
I am currently reluctant to implement *FIFO buffers* because I can’t see how these will work with asynchronous code execution on each coarray image. Instead I prefer to use asynchronous coroutines that do execute simultaneously on each coarray image, so that each coarray image does always execute a portion (kernel) of a task (coroutine). I consider usage of multiple channels simultaneously (for communication between kernels of the different asynchronous coroutines) as a buffer yet, but can’t tell if this will work as efficiently as FIFO buffers on FPGAs. Asynchronous coroutines do already work efficiently on a CPU and may qualify for a broaden range of (data flow) algorithms.

