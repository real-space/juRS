#define string_t character(len=32)
#define status_t integer
#define iounit_t integer
#define MPI_Comm integer

#ifdef DEBUG_ALL
#define DEBUG
#endif

#define USE_POINTER_IN_TYPES

#ifdef USE_POINTER_IN_TYPES
#define _allocatable_ pointer
#define _allocated_   associated
!! #define _nullify_     nullify
#else
#define _allocatable_ allocatable
#define _allocated_   allocated
!! #define _nullify_     deallocate
#endif
