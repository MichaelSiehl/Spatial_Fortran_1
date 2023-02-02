# General Purpose Data Flow (Spatial) Programming for FPGAs Using Coarray Fortran (as yet only emulated on CPUs): 1. Defining Some Basics

I open this repository to initially define and describe some basic spatial programming techniques using Coarray Fortran (2018).

*Note: The contents herein are ongoing work and preliminary. Contents may certainly contain mistakes and errors, I do make a number of assumptions and best guesses herein. As yet, all parallel programming here does work efficiently on a CPU and it is only assumed that the same programming will work on FPGAs. **Fortran would then allow to use the same programming for both CPUs and FPGAs.** This work does still describe programming for FPGAs, and even if the Fortran codes are the same for CPUs, describing these codes for programming on CPUs would be very differently.<br />
Some familiarity with Coarray Fortran as well as with spatial programming concepts (as initially described for DPC++) may be required for the reader to be able to follow with the contents here. For links to resources see the section ‘Prerequisites’ at the end.*



## 1.  Prolog

Initially described for (Spatial) DPC++ yet, new generations of FPGAs do already allow new forms of parallel (spatial) programming to deliver unknown levels of parallel performance with a very low energy consumption to hopefully establish a new era of green computing. It is assumed that some LLVM-based Fortran compilers will become spatial compilers in the near future. As yet I do use the traditional Fortran compilers (gfortran/OpenCoarrays and ifort) to emulate spatial programming with Coarray Fortran on CPUs, and do assume (making a number of assumptions) that virtually the same codes will also run on FPGAs with an upcoming spatial compiler.<br />

Coarray Fortran was obviously not specifically designed for, and is not described with respect to spatial programming for data flow architectures. It may therefore be helpful to compare basic programming concepts with (Spatial) DPC++ and to use terminology from (Spatial) DPC++ here.<br />

Coarray Fortran may have an (rather unintended) open design: I’ll utilize coarrays to customize the Fortran language to adapt it to spatial programming techniques that are initially described for (Spatial) DPC++ yet.<br />

Coarray Fortran’s design does allow to program basic SPMD coroutines (groups of kernels) on coarray images using (user-defined) traditional blocking synchronization methods. This work will extend this to ***asynchronous coroutines*** to execute multiple different coroutines (still SPMD) simultaneously on each coarray image, using a lightweight and non-blocking synchronization method (through coarray-based user-defined ***channels***).<br />

Mapping of such coroutine kernels into spatial pipeline stages and building of spatial pipelines is assumed to go through ***parallel loop instances*** on the coarray images (as well as iterations of each loop instance), as this technique does already work for emulating spatial programming on CPUs here.<br />
