#include <stdio.h>
// #include <malloc.h>
#include <stdlib.h> // also contains malloc
#include <string.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include <expat.h>
#include <math.h>
#include <errno.h>
#include <assert.h>
#include "../pawxmlreader.h"

#include <stdarg.h>

#ifndef NOMPI
  #include <mpi.h>
#endif

#define XML_FMT_INT_MOD "l"

// from https://renenyffenegger.ch/notes/development/languages/C-C-plus-plus/preprocessor/macros/__VA_ARGS__/count-arguments
#define ELEVENTH_ARGUMENT(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, ...) a11
#define COUNT_ARGUMENTS(...) ELEVENTH_ARGUMENT(dummy, ## __VA_ARGS__, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0)

#define FORT_OUTPUT

#ifdef FORT_OUTPUT
#define PRINT(format, ...) fort_printf(format, COUNT_ARGUMENTS(__VA_ARGS__), ## __VA_ARGS__)
#else
#define PRINT(format, ...) fprintf(stdout, format, ## __VA_ARGS__)
#endif

extern void print_from_fortran(const char *to_print, int *num_chars);

int fort_printf(const char* format_string, int num_args, ...) {
  static char temp[512]; //our lines have a maximum length of 512 characters
  // however, lines passed to fortran have maximum length of 128 characters
  va_list valist;
  int num_chars_written=0;
  int num_chars, num_chars_to_pass;
  const char *buffer_to_pass=&temp[0];
  va_start(valist, num_args);
  num_chars=num_chars_written=vsnprintf(temp,512+1, format_string, valist); // careful, no 0 terminator!

  do {
    num_chars_to_pass=(num_chars>128?128:num_chars);
    print_from_fortran(buffer_to_pass, &num_chars);

    buffer_to_pass+=num_chars_to_pass;
    num_chars-=num_chars_to_pass;
  } while(num_chars>=128);

  va_end(valist);
  return num_chars_written;
}


/* XML KEYS   */
#define PAW_SETUP                      1
#define ATOM                           2
#define XC_FUNCTIONAL                  3
#define GENERATOR                      4
#define AE_ENERGY                      5
#define CORE_ENERGY                    6
#define VALENCE_STATES                 7
#define STATE                          8
#define RADIAL_GRID                    9
#define SHAPE_FUNCTION                10
#define AE_CORE_DENSITY               11
#define PSEUDO_CORE_DENSITY           12
#define PSEUDO_VALENCE_DENSITY        13
#define ZERO_POTENTIAL                14
#define BLOCHL_LOCAL_IONIC_POTENTIAL  15
#define AE_PARTIAL_WAVE               16
#define PSEUDO_PARTIAL_WAVE           18
#define PROJECTOR_FUNCTION            19
#define KIN_DIFF                      20



/* buffer size */
#define MAXDEPTH                 64
#define MAX_TAGNAMES             64


#define MAX_RADIAL_EQ_STRLEN     64
#define MAX_RADIAL_ID_STRLEN     16
#define MAX_SHAPE_TYPE_STRLEN    64

#define MAX_ATOM_STATES          16
#define MAX_GENERATOR_STRLEN     128
#define MAX_FUNC_XC_STRLEN       32
#define MAX_ATOM_SYMBOL_STRLEN    4
#define MAX_RADIAL_GRIDS         16





typedef struct {
  
  char *tagname[MAX_TAGNAMES];
  int depth;

} tree_trace;

typedef struct {
  
  char *str;
  int key;

} string_key;


typedef struct {

  double a;
  double d;
  double b;
  int istart;
  int iend; //last usable index
  int n;

  char eq[MAX_RADIAL_EQ_STRLEN];
  char id[MAX_RADIAL_ID_STRLEN];
  
} radial_grid_desc;

typedef struct {
  
  double *f;
  radial_grid_desc *grid;
  int cur_widx; // current write index to append data

} radial_function;



typedef struct {

  double *f;
  int cur_widx; // current write index to append data

} windexed_buffer;

typedef struct {
  
  int n;
  int l;
  double f;
  double rc;
  double e;
  char id[16];
  radial_function ae_wave;
  radial_function pseudo_wave;
  radial_function projector_function;

} atomic_state;



typedef struct {
  
  char type[MAX_SHAPE_TYPE_STRLEN];
  double rc;
  double lambda;
  
} shape_function_compensator;


typedef struct {
  
  // if you change this part, you must also change type paw_atom in the FORTRAN interface in mod_pawxmlreader.F90 !!!!
  double zeta;
  double n_core;
  double n_valence;

  double ae_energy_kin;
  double ae_energy_xc;
  double ae_energy_es;
  double ae_energy_tot;
  double core_energy_kin;
  
  double shape_comp_rc;
  double shape_comp_lamb;
  int shape_comp_type;


  int num_states;
  int num_grids;
  // up to here--------------------------------- 


  atomic_state state[MAX_ATOM_STATES];
  radial_grid_desc grid[MAX_RADIAL_GRIDS];
  
  
  shape_function_compensator shape_function;

  radial_function ae_core_density;
  radial_function pseudo_core_density;
  radial_function pseudo_valence_density;
  radial_function zero_potential;
  radial_function blochl_local_ionic_potential;
  windexed_buffer kin_diff;

  char symbol[MAX_ATOM_SYMBOL_STRLEN];
  char xc_func_type[MAX_FUNC_XC_STRLEN];
  char xc_func_name[MAX_FUNC_XC_STRLEN];

  char generator_type[MAX_GENERATOR_STRLEN];
  char generator_name[MAX_GENERATOR_STRLEN];
  char *generator_string;
    
} paw_atom;

typedef struct {
  
  int l;
  double dr;
  int n;
  double* data;
  
} spline;

/*  file-global vars    */

static string_key valid_keys[] = {{"paw_setup", PAW_SETUP},
				  {"atom", ATOM},
				  {"xc_functional", XC_FUNCTIONAL},
				  {"generator", GENERATOR},
				  {"ae_energy", AE_ENERGY},
				  {"core_energy", CORE_ENERGY},
				  {"valence_states", VALENCE_STATES},
				  {"state", STATE},
				  {"radial_grid", RADIAL_GRID},
				  {"shape_function", SHAPE_FUNCTION},
				  {"ae_core_density", AE_CORE_DENSITY},
				  {"pseudo_core_density", PSEUDO_CORE_DENSITY},
				  {"pseudo_valence_density", PSEUDO_VALENCE_DENSITY},
				  {"zero_potential", ZERO_POTENTIAL},
				  {"blochl_local_ionic_potential", BLOCHL_LOCAL_IONIC_POTENTIAL},
				  {"ae_partial_wave", AE_PARTIAL_WAVE},
				  {"pseudo_partial_wave", PSEUDO_PARTIAL_WAVE},
				  {"projector_function", PROJECTOR_FUNCTION},
				  {"kinetic_energy_differences", KIN_DIFF},
				  {"", 0},
				  
				  
};



static string_key shape_fun_keys[] = {{"gauss", PAWXMLREADER_SHAPE_GAUSS},
				      {"bessel", PAWXMLREADER_SHAPE_BESSEL},
				      {"sinc", PAWXMLREADER_SHAPE_SINC},
				      {"exp", PAWXMLREADER_SHAPE_EXP},
				      {"", 0},
				 
};

// our atom object, contains everything
static paw_atom atom;

// traces the xml tree
static tree_trace trace;

//tells the kind of the incoming data, if 0 no relevant tag
//it uses the tag definitions/numbers used in the switch case function 
// it is set in the tag handler functions to the corrisponding tag value
static int data_kind; 

// points to the current atomic state such that when data arrives from the parser 
// we can append floating point numbers to the correct array 
static atomic_state *current_state;

static int myrank;

static int write_waves = 0;

static spline spline_build(int l, double dr, int nbins, double* f) {
  double c;
  double *f2;
  double *u;
  double *data;
  int b;
  
  c = 3.0 / (dr * dr);
  f2 = (double*)malloc((nbins + 1) * sizeof(double));
  assert(f2 != NULL);
  u = (double*)malloc(nbins * sizeof(double));
  assert(u != NULL);
  f2[0] = -0.5;
  u[0] = (f[1] - f[0]) * c;
  
  for (b = 1; b < nbins; b++) {  
      double p = 0.5 * f2[b - 1] + 2.0;
      f2[b] = -0.5 / p;
      u[b] = ((f[b + 1] - 2.0 * f[b] + f[b - 1]) * c - 0.5 * u[b - 1]) / p;
  }
  
  f2[nbins] = ((f[nbins - 1] * c - 0.5 * u[nbins - 1]) /
               (0.5 * f2[nbins - 1] + 1.0));
  
  for (b = nbins - 1; b >= 0; b--) {
    f2[b] = f2[b] * f2[b + 1] + u[b];
  }
  
  data = (double*)malloc(4 * (nbins + 1) * sizeof(double));
  assert(data != NULL);
  
  spline sp = {l, dr, nbins, data};
  
  for (b = 0; b < nbins; b++) {
      *data++ = f[b];
      *data++ = (f[b + 1] - f[b]) / dr - (f2[b] / 3 + f2[b + 1] / 6) * dr;
      *data++ = 0.5 * f2[b];
      *data++ = (f2[b + 1] - f2[b]) / (6 * dr);
  }
  
  data[0] = 0.0;
  data[1] = 0.0;
  data[2] = 0.0;
  data[3] = 0.0;
  free(u);
  free(f2);
  return sp;
}


static double spline_value(const spline* sp, double r) {
  int b = r / sp->dr;
  if (b >= sp->n)
    return 0.0;
  double u = r - b * sp->dr;
  double* s = sp->data + 4 * b;
  return  s[0] + u * (s[1] + u * (s[2] + u * s[3]));
}

static void spline_free(spline* sp) {
  free(sp->data);
}



void pawxml_fill_paw_atom_(paw_atom *pa) {
  memcpy(pa, &atom, 10*sizeof(double)+3*sizeof(int));
}


void pawxml_fill_paw_atom(paw_atom *pa) {
  pawxml_fill_paw_atom_(pa);
}

// get the number of projectors (states)  for each channel ell
int pawxml_get_number_of_projectors(int ell) {
  int i;
  int num;
  
  num = 0;  
  for(i=0;i<atom.num_states;i++) {
    if(atom.state[i].l == ell) {
	num++;
    }
  }
  return num;
}

// get the  projector rcut for each channel ell
double pawxml_get_projector_rcut(int ell) {
  double rcut;
  int i;
  
  rcut = 0.0;  
  for(i=0;i<atom.num_states;i++) {
    if(atom.state[i].l == ell) {
      rcut = atom.state[i].rc;
      return rcut;
    }
  }
  return rcut;
}


//enn is not the real n of the state, it is an index 
//just to index the different states for channel ell
static atomic_state *get_state(int enn, int ell) {
  int i;
  int cnt = 0;
  // given ell, if enn == 1 give the first state with that ell
  // if enn == 2 give the second and so on
  for(i=0;i<atom.num_states;i++) {
    if(atom.state[i].l == ell) {
      cnt++;
      if(enn == cnt) {
	return &atom.state[i];
      }
    }
  }
  
  PRINT("no match in get_state in pawxmlreader, aborting..\n");
  exit(1);
}

static int get_state_index(int enn, int ell) {
    return (get_state(enn, ell) - atom.state); 
}


// enn is not the real enn, just an index from 1 to num-of-states(ell)
double pawxml_get_state_ae_energy(int enn, int ell) {
  atomic_state *state;
  
  state =  get_state(enn, ell);
  //PRINT("energy of state %d %d = %e\n", enn, ell, state->e);
  return state->e;
}


double pawxml_get_state_occupation(int enn, int ell) {
  atomic_state *state;
  
  state =  get_state(enn, ell);
  return state->f;
}


double pawxml_get_kin_diff(int enn, int ell, int en2, int el2 ) {
  int idx1, idx2;
  int offset;
  
  idx1 = get_state_index(enn, ell);
  idx2 = get_state_index(en2, el2);
  
  offset = idx2 * atom.num_states + idx1;
  
  return atom.kin_diff.f[offset];
}

void pawxml_get_generator_string(char *buf) {
  strncpy(buf,atom.generator_string,MAX_GENERATOR_STRLEN);
  for(int i=strlen(atom.generator_string);i<MAX_GENERATOR_STRLEN;++i)buf[i]=' ';
}



static FILE *open_file(char *fname) {
  FILE *f;
  //PRINT("opening input xml file %s\n", fname);
  f = fopen(fname, "r");
  if(f == NULL) {
    PRINT("error opening file '%s'\n", fname);
    PRINT("%s\n", strerror(errno));
    exit(1);
  }
  //PRINT("file opened \n");
  return f;
}

static int file_size(char *fname) {
  int sz;
  struct stat st;
  stat(fname, &st);
  sz = st.st_size;

  return sz;
}

/*
static int current_tag_is(const char *tag) {
  if(strcmp(tag, trace.tagname[trace.depth-1])==0) {
    return 1;
  } 
  else {
    return 0;
  }
}
*/

static int prev_tag_is(const char *tag) {
  if(strcmp(tag, trace.tagname[trace.depth-2])==0) {
    return 1;
  } 
  else {
    return 0;
  }
}


void pawxml_reader_cleanup() {
  int i;
#define free_if_allocated(pointer) if(pointer) free(pointer);  
  free_if_allocated(atom.ae_core_density.f)
  free_if_allocated(atom.pseudo_core_density.f)
  free_if_allocated(atom.pseudo_valence_density.f)
  free_if_allocated(atom.zero_potential.f)
  free_if_allocated(atom.blochl_local_ionic_potential.f)
  free_if_allocated(atom.kin_diff.f)
  //now states
  for(i=0;i<atom.num_states;i++) {
    free_if_allocated(atom.state[i].ae_wave.f)
    free_if_allocated(atom.state[i].pseudo_wave.f)
    free_if_allocated(atom.state[i].projector_function.f)
  }
  free_if_allocated(atom.generator_string);
#undef free_if_allocated
}


static int grid_id_to_index(const char *id) {
  int idx;
  int ng;
  int i;

  ng = atom.num_grids;
  
  for(i=0;i<ng;i++) {
    if(strcmp(id, atom.grid[i].id)==0) {
      //PRINT("found grid idx %d corresponding to id=%s\n", i, id);
      idx = i;
      return idx;
    }
  }

  PRINT("failed to find grid id in grids.... panicking :O oOooOOOO\n");
  exit(1);
  return -1;
}

static int element_to_key(string_key *vk, const char *el) { 
  int i;
  
  for(i=0; vk[i].key!=0; i++) {
    if(!strcmp((const char *)vk[i].str, (const char *)el)) return vk[i].key;
  }
  
  return 0; // no valid keys matched

}


static void handle_paw_setup(const char **attr) {
  
  if(strcmp(attr[0],"version")==0) {
    if(!myrank)
      PRINT("reading a paw setup version %s  mumble...\n", attr[1]);
  }
  
}

static void handle_atom(const char **attr) {

  int i;

  for (i = 0; attr[i]; i += 2) {

    if(strcmp("symbol", attr[i])==0) {
      strncpy(atom.symbol, attr[i+1], 2);
    }
    
    if(strcmp("Z", attr[i])==0) {
      atom.zeta = atof(attr[i+1]);
    }
    
    if(strcmp("core", attr[i])==0) {
      atom.n_core = atof(attr[i+1]);
    }
    
    if(strcmp("valence", attr[i])==0) {
      atom.n_valence = atof(attr[i+1]);
    }
    
    //PRINT(" %s='%s'", attr[i], attr[i + 1]);
  }


  //PRINT("got atom %s, Z = %.0f, core = %.0f, val = %.0f\n", atom.symbol, atom.zeta, atom.n_core, atom.n_valence);

}


static void handle_xc_functional(const char **attr) {

  int i;

  for (i = 0; attr[i]; i += 2) {
    
    if(strcmp("type", attr[i])==0) {
      strncpy(atom.xc_func_type, attr[i+1], MAX_FUNC_XC_STRLEN);
    }

    if(strcmp("name", attr[i])==0) {
      strncpy(atom.xc_func_name, attr[i+1], MAX_FUNC_XC_STRLEN);
    }
  }
  
  //PRINT("got xc functional type %s, name %s\n", atom.xc_func_type, atom.xc_func_name);
}

static void handle_ae_energy(const char **attr) {

  int i;

  for (i = 0; attr[i]; i += 2) {

    if(strcmp("kinetic", attr[i])==0) {
      atom.ae_energy_kin = atof(attr[i+1]);
    }
    
    if(strcmp("xc", attr[i])==0) {
      atom.ae_energy_xc = atof(attr[i+1]);
    }
    
    if(strcmp("electrostatic", attr[i])==0) {
      atom.ae_energy_es = atof(attr[i+1]);
    }

    if(strcmp("total", attr[i])==0) {
      atom.ae_energy_tot = atof(attr[i+1]);
    }
    
  }
  
  // PRINT("got ae data : kin = %e, xc = %e, es = %e, tot = %e \n", 
  // atom.ae_energy_kin, atom.ae_energy_xc, atom.ae_energy_es, atom.ae_energy_tot);

}


static void handle_generator(const char **attr) {

  int i;

  for (i = 0; attr[i]; i += 2) {
     
    if(strcmp("type", attr[i])==0) {
      strncpy(atom.generator_type, attr[i+1], MAX_GENERATOR_STRLEN);
    }

    if(strcmp("name", attr[i])==0) {
      strncpy(atom.generator_name, attr[i+1], MAX_GENERATOR_STRLEN);
    }
  }

  atom.generator_string=calloc(1,sizeof(char));
  
  // PRINT("got generator : type = %s , name = %s\n", 
  //	 atom.generator_type, atom.generator_name);
  data_kind = GENERATOR;
}

static void handle_core_energy(const char **attr) {

  int i;

  for (i = 0; attr[i]; i += 2) {
    if(strcmp("kinetic", attr[i])==0) {
      atom.core_energy_kin = atof(attr[i+1]);
    }
  }
  
  // PRINT("got core data : kin = %e \n", 
  //	 atom.core_energy_kin);

}



static void handle_state(const char **attr) {

  int i;
  atomic_state *state;
  
  // check that we are in the right section
  if(!prev_tag_is("valence_states")) {
    PRINT("found atomic state outside valence states... will not handle this");
    return;
  }
  
  
  state = &atom.state[atom.num_states]; //use current free state
  
  for (i = 0; attr[i]; i += 2) {
    if(strcmp("n", attr[i])==0) {
      state->n = atoi(attr[i+1]);
    }
    if(strcmp("l", attr[i])==0) {
      state->l = atoi(attr[i+1]);
    }
    if(strcmp("f", attr[i])==0) {
      state->f = atof(attr[i+1]);
    }
    if(strcmp("rc", attr[i])==0) {
      state->rc = atof(attr[i+1]);
    }    
    if(strcmp("e", attr[i])==0) {
      state->e = atof(attr[i+1]);
    }
    if(strcmp("id", attr[i])==0) {
      strncpy(state->id, attr[i+1], 16);
    }
  }
  
  //PRINT("got state : n = %d l = %d f = %e rc = %e e = %e id = %s\n", 
  // state->n, state->l, state->f, state->rc, state->e, state->id);
  
  atom.num_states++; //increment, next handle_state call will use a free state
}


static void handle_radial_grid(const char **attr) {
  int i;
  radial_grid_desc *grid;
    
  grid = &atom.grid[atom.num_grids];
  
  for (i = 0; attr[i]; i += 2) {

    if(strcmp("eq", attr[i])==0) {
      strncpy(grid->eq, attr[i+1], MAX_RADIAL_EQ_STRLEN);
    }
    
    if(strcmp("id", attr[i])==0) {
      strncpy(grid->id, attr[i+1], MAX_RADIAL_ID_STRLEN);
    }
    if(strcmp("a", attr[i])==0) {
      grid->a = atof(attr[i+1]);
    }
    if(strcmp("d", attr[i])==0) {
      grid->d = atof(attr[i+1]);
    }
     if(strcmp("b", attr[i])==0) {
      grid->b = atof(attr[i+1]);
    }
    if(strcmp("istart", attr[i])==0) {
      grid->istart = atoi(attr[i+1]);
    }

    if(strcmp("iend", attr[i])==0) {
      grid->iend = atoi(attr[i+1]);
    }
   
    if(strcmp("n", attr[i])==0) {
      grid->n = atoi(attr[i+1]);
    }
  }

  /*
  PRINT("got a grid eq = %s, a=%e, d=%e, istart=%d, iend=%d, id=%s", 
	 grid->eq, grid->a, grid->d, grid->istart, grid->iend, grid->id);
  */

  atom.num_grids++;
  if(atom.num_grids > 1) {
    PRINT("pseudopotentials with more than 1 grid are not yet supported \n");
    exit(0);
  }

}

static void handle_shape_function(const char **attr) {
  int i;

  for (i = 0; attr[i]; i += 2) {
    
    if(strcmp("type", attr[i])==0) {
      strncpy(atom.shape_function.type, attr[i+1], MAX_SHAPE_TYPE_STRLEN);
      atom.shape_comp_type =  element_to_key(shape_fun_keys, atom.shape_function.type);
      //PRINT("got key for shape type %d\n", atom.shape_comp_type);
    }

    if(strcmp("rc", attr[i])==0) {
      atom.shape_function.rc = atof(attr[i+1]);
      atom.shape_comp_rc = atom.shape_function.rc;
    }
    
    if(strcmp("lamb", attr[i])==0) {
      atom.shape_function.lambda = atof(attr[i+1]);
      atom.shape_comp_lamb = atom.shape_function.lambda;
    }

  }

}

static void init_radial_function(radial_function *rf, int idx) { //idx = grid index
  rf->f = (double*) malloc(sizeof(double)*(atom.grid[idx].iend - atom.grid[idx].istart +1));
  rf->grid = &atom.grid[idx];
  rf->cur_widx = 0;
}


static void handle_radial_function(const char **attr, int key, radial_function *rf) {
  int i;
  int grid_idx;

  
  //PRINT("handling radial function with key %d\n", key);

  for (i = 0; attr[i]; i += 2) {
    if(strcmp("grid", attr[i])==0) {
      grid_idx = grid_id_to_index(attr[i+1]);
    }
  }
  
  // allocate the buffer to hold doubles.

  init_radial_function(rf, grid_idx);
  
  
  //expect data of this kind  
  data_kind = key;
  //PRINT("got wf with key %d, grid = %d %s\n", key, grid_idx, atom.grid[grid_idx].id);
  

}

static atomic_state *state_from_id(const char *id) {
  int i;
  atomic_state *state;

  for(i=0;i<atom.num_states;i++) {
    state = &atom.state[i];
    if(strcmp(id, state->id)==0) {
      return state;
    }
    
  }

  PRINT("cannot find state with id %s, panicking :O\n", id);
  exit(1);
  return 0;

}



static void handle_radial_state(const char **attr, int key) {
  int i;
  int grid_idx;
  atomic_state *state;
  radial_function *rf;
  //find the state and grid
  
  //PRINT("handling radial state with key %d\n", key);

  for (i = 0; attr[i]; i += 2) {
    if(strcmp("state", attr[i])==0) {
      state = state_from_id(attr[i+1]);  
    }
    
    if(strcmp("grid", attr[i])==0) {
      grid_idx = grid_id_to_index(attr[i+1]);
    }
  }

  //now we have state and grid
  //get the correct radial function

  switch (key) {

  case AE_PARTIAL_WAVE:
    rf = &state->ae_wave;
    break;

  case PSEUDO_PARTIAL_WAVE:
    rf = &state->pseudo_wave;
    break;
    
  case PROJECTOR_FUNCTION:
    rf = &state->projector_function;
    break;
    
  default :
    PRINT("cannot get proper radial function while handling state, panicking :O\n");
    exit(1);
  }
  
  // allocate the buffer to hold doubles
  init_radial_function(rf, grid_idx);
  
  
  //expect data of this kind  
  data_kind = key;
  current_state = state;
  //PRINT("got wf with key %d, grid = %d %s\n", key, grid_idx, atom.grid[grid_idx].id);
  

}

static void handle_kin_diff() {
  data_kind = KIN_DIFF; //set data kind
  // allocate buffer (matrix) num_states*num_states
  atom.kin_diff.f = (double*) malloc(sizeof(double)*atom.num_states*atom.num_states);
  atom.kin_diff.cur_widx = 0; //set write index to 0
}


static void append_to_radial(const char *tok, radial_function *rf) {
  
  if(rf->cur_widx > rf->grid->iend) {
    PRINT("too much data in radial function \n");
  } else {
    // add floating point data to the array
    rf->f[rf->cur_widx] = atof(tok);
    rf->cur_widx++;  
    //PRINT("got token %s ", tok);
  }
  
}

static void append_to_wbuf(const char *tok, windexed_buffer *wb) {

  if(data_kind == KIN_DIFF) {
  // add floating point data to the array
    wb->f[wb->cur_widx] = atof(tok);
    wb->cur_widx++;
  }
  
}

static void append_char_data(const char *buf, char **wb) {
  char *temp;
  temp = calloc(strlen(buf)+strlen(*wb)+1, sizeof(char));
  if(temp == NULL) PRINT("Could not allocate memory for appending char data");
  strcpy(temp, *wb);
  strcpy(temp+strlen(*wb),buf);
  free(*wb);
  *wb=temp;
  return;
}



static void append_data(const char *buf, radial_function *rf, windexed_buffer *wbuf) {
  char *str;
  char *tok;
  char *strmem;
  
  
  //tokenize buf
 
  strmem = malloc(strlen(buf)+1);
  strcpy(strmem, buf);
  str = strmem;

  tok = str;
  while (tok != NULL) {
    
    tok = strtok(str, " ");
    str = NULL;
    
    if(tok) {

      if(rf)
	append_to_radial(tok, rf);
      
      if(wbuf)
	append_to_wbuf(tok, wbuf);
       
    } // end if token exists
    
  } // end tokenizing
  
  free(strmem);
  
}



static void handle_data(void *data, const char *content, int len) {

  char *buf;

  if(len>1) {
    
    buf = malloc(len+1);  
    memcpy(buf, content, len);
    buf[len] = 0;
     

    switch(data_kind) {

    case AE_CORE_DENSITY:
      append_data(buf, &atom.ae_core_density, NULL);
      break;
   
    case PSEUDO_CORE_DENSITY:
      append_data(buf, &atom.pseudo_core_density, NULL);
      break;
      
    case PSEUDO_VALENCE_DENSITY:
      append_data(buf, &atom.pseudo_valence_density, NULL);
      break;

    case ZERO_POTENTIAL:
      append_data(buf, &atom.zero_potential, NULL);
      break;
      
    case BLOCHL_LOCAL_IONIC_POTENTIAL:
      append_data(buf, &atom.blochl_local_ionic_potential, NULL);
      break;
    
    case AE_PARTIAL_WAVE:
      append_data(buf, &current_state->ae_wave, NULL);
      break;

    case PSEUDO_PARTIAL_WAVE:
      append_data(buf, &current_state->pseudo_wave, NULL);
      break;
      
    case PROJECTOR_FUNCTION:
      append_data(buf, &current_state->projector_function, NULL);
      break;
      
    case GENERATOR:
      append_char_data(buf, &atom.generator_string);
      break;
     
    case KIN_DIFF:
      append_data(buf, NULL, &atom.kin_diff);
      break;
      
    default:
      break;
    }
  
    free(buf);
  }
    
}


static int handle_key(int key, const char **attr) {

  switch(key) {
    
  case PAW_SETUP:   
    handle_paw_setup(attr);
    return 1;
    break;
    
  case ATOM:
    handle_atom(attr);
    return 1;
    break;
    
  case XC_FUNCTIONAL:
    handle_xc_functional(attr);
    return 1;
    break;
    
  case GENERATOR:
    handle_generator(attr);
    return 1;
    break;

  case AE_ENERGY:
    handle_ae_energy(attr);
    return 1;
    break;
    
  case CORE_ENERGY:
    handle_core_energy(attr);
    return 1;
    break;
    
  case STATE:
    handle_state(attr);
    return 1;
    break;

  case RADIAL_GRID:
    handle_radial_grid(attr);
    return 1;
    break;

  case SHAPE_FUNCTION:
    handle_shape_function(attr);
    return 1;
    break;

  case AE_CORE_DENSITY:
    handle_radial_function(attr, AE_CORE_DENSITY, &atom.ae_core_density);
    return 1;
    break;

  case PSEUDO_CORE_DENSITY:
    handle_radial_function(attr, PSEUDO_CORE_DENSITY, &atom.pseudo_core_density);
    return 1;
    break;

  case PSEUDO_VALENCE_DENSITY:
    handle_radial_function(attr, PSEUDO_VALENCE_DENSITY, &atom.pseudo_valence_density);
    return 1;
    break;

  case ZERO_POTENTIAL:
    handle_radial_function(attr, ZERO_POTENTIAL, &atom.zero_potential);
    return 1;
    break;
    
  case BLOCHL_LOCAL_IONIC_POTENTIAL:
    handle_radial_function(attr, BLOCHL_LOCAL_IONIC_POTENTIAL, &atom.blochl_local_ionic_potential);
    return 1;
    break;


  case AE_PARTIAL_WAVE:
  case PSEUDO_PARTIAL_WAVE:
  case PROJECTOR_FUNCTION:
    handle_radial_state(attr, key);
    return 1;
    break;

  case VALENCE_STATES:
    return 1;
    break;

  case KIN_DIFF:
    handle_kin_diff();
    return 1;
    break;


  default:
    return 0;
    break;

  }
}

// called by the parser when a tag is opened, data is (unused) user data
// el is the tag name, attr contains attributes attr[2*i] is attribute i  name, 
// attr[2*i+1] is the value of attribute i 

static void XMLCALL start(void *data, const char *el, const char **attr) {
  
  int key;
  int d;
  int valid;

  valid = 0;
  
  // keep track of where we are 
  d = trace.depth;
  trace.tagname[d] = (char*) malloc(strlen(el)+1); //freed in end
  strcpy(trace.tagname[d], el);
  trace.depth += 1;

  key = element_to_key(valid_keys, el);

  if(key) {

      valid = handle_key(key, attr);
  } 

  if(!valid) {
        if(!myrank)
	  PRINT("WARNING: found a key which is not explicitly handled: \"%s\"\n", el);
  }
  
}


// called by the parser when a tag is closed
static void XMLCALL end(void *data, const char *el)
{
  data_kind = 0; //set to zero data_kind as the tag closed
  trace.depth -= 1;
  free(trace.tagname[trace.depth]);
}



static void XMLCALL comment_handler(void *userData, const XML_Char *data) {
  //PRINT("comment inside xml says:\n\n");
  //PRINT("%s", data);
  //PRINT("\n\n\n");
}
  

static double grid_at(const radial_grid_desc *gr, int i) {

  //if(strcmp(gr->eq, "r=a*(exp(d*i)-1)")==0) {  
  //  return gr->a*(exp(gr->d*(double)i)-1.0);
  //} 
  //else 
  if(strcmp(gr->eq, "r=a*i/(n-i)")==0) {
    return gr->a*((double)i)/((double)(gr->n-i));
  } else if(strcmp(gr->eq, "r=a*(exp(d*i)-1)")==0) {  
    return gr->a*(exp(gr->d*(double)i)-1.0);
  } else {
    PRINT("cannot handle this grid yet %s aborting\n", gr->eq);
    exit(1);
  }
  
  /*
  else if(strcmp(gr->eq, "r=a*i/(1-b*i)")==0) {
    return gr->a*((double)i)/((1.0-gr->b*(double)i));
  } 
  else if(strcmp(gr->eq, "r=d*i")==0) {  
    return gr->d*(double)i;
  } 
  else if(strcmp(gr->eq, "r=(i/n+a)^5/a-a^4")==0) {  
    return pow((i/gr->n+gr->a), 5) / gr->a - pow(gr->a, 4);
  } 
  else if(strcmp(gr->eq, "r=a*exp(d*i)")==0) {
    return gr->a*(exp(gr->d*(double)i)); 
  }
  
  else {
    PRINT("cannot handle this grid yet %s aborting\n", gr->eq);
    exit(1);
  }
  */
  
  return (double)i;

}



static void print_radial(const radial_function *rf, const char *fname) {
  int i;
  int ist, iend;
  FILE *fout;
  
  if(!write_waves) return;
    
  if(!rf->f) return;
  
  fout = stdout;
  
  if(fname) {
    fout = fopen(fname, "w");
  }
  
  ist = rf->grid->istart;
  iend = rf->grid->iend;

  for(i=ist;i<=iend;i++) {
    fprintf(fout, "%e %e\n", grid_at(rf->grid, i), rf->f[i-ist]);
  }

  if(fname) {
    fclose(fout);
  }
  /*
  for(i=iend;i>=ist;i--) {
    if(fabs(rf->f[i-ist]) > 1e-10 ) {
        PRINT("%s actual rcut = %f\n", fname, grid_at(rf->grid, i));
        break;
    } 
  }
  */
}
                            

static void show_atom() {

  int i, j;
  atomic_state *state;
  radial_grid_desc *grid;
  char fname[80];
  double ev = 27.211385;
  
  PRINT("\n\nATOM DATA:\n");
  PRINT("\t atom %s, Z = %.0f, core = %.0f, val = %.0f\n", atom.symbol, atom.zeta, atom.n_core, atom.n_valence);
  PRINT("\t xc functional type %s, name %s\n", atom.xc_func_type, atom.xc_func_name);
  PRINT("\t generator : type = %s , name = %s\n", atom.generator_type, atom.generator_name);
  PRINT("\t generator string: %s\n", atom.generator_string);
  PRINT("\t ae data : kin = %e, xc = %e, es = %e, tot = %e \n", 
	 atom.ae_energy_kin, atom.ae_energy_xc, atom.ae_energy_es, atom.ae_energy_tot);
  
   PRINT("\t ae data (eV): kin = %.4f, xc = %.4f, es = %.4f, tot = %.4f \n", 
         atom.ae_energy_kin*ev, atom.ae_energy_xc*ev, atom.ae_energy_es*ev, atom.ae_energy_tot*ev);
  
  PRINT("\t core data : kin = %e \n", 
	 atom.core_energy_kin);
  
  PRINT("\t found the following %d states:\n", atom.num_states);
  for(i=0;i<atom.num_states;i++) {
    state = &atom.state[i];
    PRINT("\t\t state %d: id = %s, n=%d, l=%d, f=%.2e, rc=%.4e, e=%.2e\n", 
	   i, state->id, state->n, state->l, state->f, state->rc, state->e);
  }
  
  PRINT("\t found the following %d grids:\n", atom.num_grids);
  
  for(i=0;i<atom.num_grids;i++) {
    grid = &atom.grid[i];
    PRINT("\t\t grid %d: id = %s, eq: %s, a=%.2e, d=%.2e, istart=%d, iend=%d n=%d\n", 
	   i, grid->id, grid->eq, grid->a, grid->d, grid->istart, grid->iend, grid->n);
  }
  
  PRINT("\t shape function: type=%s, rc=%.2e lambda=%.2e\n", 
	 atom.shape_function.type, atom.shape_function.rc, atom.shape_function.lambda);
  
  sprintf(fname, "%s_ae_core_density", atom.symbol) ;
  print_radial(&atom.ae_core_density, fname);
  sprintf(fname, "%s_pseudo_core_density", atom.symbol) ;
  print_radial(&atom.pseudo_core_density, fname);
  sprintf(fname, "%s_pseudo_valence_density", atom.symbol) ;
  print_radial(&atom.pseudo_valence_density, fname);
  sprintf(fname, "%s_zero_potential", atom.symbol) ;
  print_radial(&atom.zero_potential, fname);
  sprintf(fname, "%s_blochl_local_ionic_potential", atom.symbol) ;
  print_radial(&atom.blochl_local_ionic_potential, fname);
  
  
  for(i=0;i<atom.num_states;i++) {
    sprintf(fname, "%s_state_%d_%d_ae_wave",  atom.state[i].id, atom.state[i].n, atom.state[i].l);
    print_radial(&atom.state[i].ae_wave, fname);
  }

  for(i=0;i<atom.num_states;i++) {
    sprintf(fname, "%s_state_%d_%d_pseudo_wave", atom.state[i].id, atom.state[i].n, atom.state[i].l);
    print_radial(&atom.state[i].pseudo_wave, fname);
  }

  for(i=0;i<atom.num_states;i++) {
    sprintf(fname, "%s_state_%d_%d_projector", atom.state[i].id, atom.state[i].n, atom.state[i].l);
    print_radial(&atom.state[i].projector_function, fname);
  }


  PRINT("\t kinetic energy matrix\n");
  for(i=0;i<atom.num_states;i++) {
    PRINT("\t\t ");
    for(j=0;j<atom.num_states;j++) {
      
      PRINT("%9.2e ", atom.kin_diff.f[i*atom.num_states+j]);
      
    }
    PRINT("\n");
  }
  
  
  
}


static double interpolate_parabolic(const double *xval, const double *yval, double x) {
  double x1, x2, x3, y1, y2, y3;
  double denom;
  double pa, pb, pc;

  x1 = xval[0];
  x2 = xval[1];
  x3 = xval[2];

  y1 = yval[0];
  y2 = yval[1];
  y3 = yval[2];

  denom = (x1 - x2)*(x1 - x3)*(x2 - x3);
 
  pa = (x3 * (y2 - y1) + x2 * (y1 - y3) + x1 * (y3 - y2)) / denom;
  pb = (x3*x3 * (y1 - y2) + x2*x2 * (y3 - y1) + x1*x1 * (y2 - y3)) / denom;
  pc = (x2 * x3 * (x2 - x3) * y1 + x3 * x1 * (x3 - x1) * y2 + x1 * x2 * (x1 - x2) * y3) / denom;

  return (pa*x + pb)*x + pc;

}

//evaluates the lagrange polynomial small ell (h here) h contains a number (npts) off ells,
//hprime the derivative evaluated at the point x
// h and hprime are output, xpts ar the coordinates of the control points (typ. gll points)
static void lagrange_eval(double x, int npts, double *xpts, double *h, double *hprime) {
    int j,m,k;
    double prod1,prod2;

    for(j=0;j<npts;j++) {
        
        prod1 = 1.0;
        prod2 = 1.0;
        for(m=0;m<npts;m++) {
            
            if(m != j) {
                prod1 *= (x-xpts[m]);
                prod2 *= (xpts[j]-xpts[m]);
            }
        }

        h[j]=prod1/prod2;

        if(hprime) {
            hprime[j]=0.0;     
            for(m=0;m<npts;m++) {
                if(m != j) {
                    prod1=1.0;
                    
                    for(k=0;k<npts;k++) {
                        if( (k != j) && (k != m)) prod1 = prod1*(x-xpts[k]);
                    }
                    hprime[j]+= prod1;
                }
            }
            hprime[j] = hprime[j]/prod2;
        }
    }
}

#define MIN(a,b) (a) <= (b) ? (a) : (b)
#define MAX(a,b) (a) >= (b) ? (a) : (b)

static double interpolate_cubic(double *xval, const double *yval, double x) {
  double h[4];
    
  lagrange_eval(x, 4, xval, h, NULL);
  return h[0]*yval[0] + h[1]*yval[1] + h[2]*yval[2] + h[3]*yval[3];
  
}

static double interpolate_quartic(double *xval, const double *yval, double x) {
  double h[5];
    
  lagrange_eval(x, 5, xval, h, NULL);
  return h[0]*yval[0] + h[1]*yval[1] + h[2]*yval[2] + h[3]*yval[3]+h[4]*yval[4];
  
}

#define USE_INTERP

#ifdef USE_INTERP

static void eval_radial_function(const radial_function *rf, const double *r, double *f, int n) {
  int i, j, k;
  int gidxmin, gidxmax;
  int kmin;
  double r1, r2;
  
  double xv[4];
  double yv[4];
  
  //given r[i] eval the function at r[i] in the most easy way
  
  gidxmin = rf->grid->istart;
  gidxmax = rf->grid->iend;
  
  for(i=0;i<n;i++) {

    // check if r[i] is < rmin or > rmax
    if(r[i]<=grid_at(rf->grid, gidxmin)) {
      f[i] = rf->f[0];
    } else if(r[i]>=grid_at(rf->grid, gidxmax)) {
      f[i] = rf->f[gidxmax-gidxmin];
    } else {
      
      for(k=gidxmin;k<gidxmax;k++) {
	r1 = grid_at(rf->grid, k);
	r2 = grid_at(rf->grid, k+1);
	
	if( (r1<=r[i]) && (r2>r[i]) ) { // found k
	 
	  kmin = MIN(k-1, gidxmax -3);
	  kmin = MAX(kmin, gidxmin);
	 
          //printf("kmin=%d k=%d gridatkmin=%e r[i]=%e\n", kmin, k,grid_at(rf->grid, kmin), r[i]);
	  for(j=0;j<4;j++) {
	    xv[j] = grid_at(rf->grid, kmin+j);
	    yv[j] = rf->f[kmin+j-gidxmin];
	  }
	  
          
          //if(kmin == gidxmin) {
          //    f[i] = interpolate_parabolic(xv, yv, r[i]);
          //} else {
            f[i] = interpolate_cubic(xv, yv, r[i]);
          //}
          
	  break; // end loop on k and go fw with next i
	
	} //end found k

      } //end loop  k
      if(k==gidxmax) {
        PRINT("failed to find k in eval_radial_function \n");
        exit(0);
      }
    } //end different cases (borders/ bulk)
    
  } //end loop on i
  
  //exit(0);
  
  /*
  static int kk;
  
  FILE *fout;
  char fname[80];
  sprintf(fname, "radial%d", kk);
  fout = fopen(fname, "w");
  for(i=0;i<n;i++) {
    fprintf(fout, "%e %e \n", r[i], f[i]);
  }
  fclose(fout);
  
  sprintf(fname, "radial_ori%d", kk);
  fout = fopen(fname, "w");
  print_radial(rf, fname);
  fclose(fout);
  
  kk++;
  */
}

#else


// this works only when JURS uses the same grid as in the xml file!!! jurs gets the params by calling "pawxml_get_grid"
static void eval_radial_function(const radial_function *rf, const double *r, double *f, int n) {
  int i;
  
  for(i=0;i<n;i++) {  
    f[i] = rf->f[i];
  }

}

#endif

static void eval_radial_function_linear(const radial_function *rf, const double *r, double *f, int n) {
  int i, j, k;
  int gidxmin, gidxmax;
  int kmin;
  double r1, r2;
  
  double xv[4];
  double yv[4];
  
  //given r[i] eval the function at r[i] in the most easy way
  
  gidxmin = rf->grid->istart;
  gidxmax = rf->grid->iend;
  
  for(i=0;i<n;i++) {

    // check if r[i] is < rmin or > rmax
    if(r[i]<=grid_at(rf->grid, gidxmin)) {
      f[i] = rf->f[0];
    } else if(r[i]>=grid_at(rf->grid, gidxmax)) {
      f[i] = rf->f[gidxmax];
    } else {
      
      for(k=gidxmin;k<gidxmax;k++) {
        r1 = grid_at(rf->grid, k);
        r2 = grid_at(rf->grid, k+1);
        
        if( (r1<=r[i]) && (r2>r[i]) ) { // found k
         /*
          kmin = MIN(k-1, gidxmax -2);
          kmin = MAX(kmin, gidxmin);
         
          for(j=0;j<4;j++) {
            xv[j] = grid_at(rf->grid, kmin+j);
            yv[j] = rf->f[kmin+j];
          }
          
          f[i] = interpolate_cubic(xv, yv, r[i]);
          */
          yv[0] = rf->f[k];
          yv[1] = rf->f[k+1];
          f[i] = yv[0] + (r[i]-r1)*(yv[1] - yv[0])/(r2 -r1);
          
          break; // end loop on k and go fw with next i
        
        } //end found k

      } //end loop  k
      if(k==gidxmax) {
        PRINT("failed to find k in eval_radial_function \n");
        exit(0);
      }
    } //end different cases (borders/ bulk)
    
  } //end loop on i
  
}



// n is number of grid points
void pawxml_get_rho_core(double *rho_smoot, double *rho_true, const double *grid, int n, double *cm) {
  
  eval_radial_function(&atom.ae_core_density, grid, rho_true, n);
  eval_radial_function(&atom.pseudo_core_density, grid, rho_smoot, n);
  
  
  
  
  double zcore;
  int i, ist, iend;
  int ngrid;
  int rgrid_type;
  double edi;
  double r, r1, r2, dr, a, b, g;
  radial_function *rf;
  rf = &atom.ae_core_density;
  
  ist = rf->grid->istart;
  iend = rf->grid->iend;
  
  
  zcore = 0.0;
  
  
  g = rf->grid->n;
  a = rf->grid->a/g;
  b = 1.0/g;

  
  if(strcmp(rf->grid->eq, "r=a*i/(n-i)")==0) {
    // return gr->a*((double)i)/((double)(gr->n-i));
    rgrid_type = 1;
  } else if(strcmp(rf->grid->eq, "r=a*(exp(d*i)-1)")==0) {  
    // return gr->a*(exp(gr->d*(double)i)-1.0);
    rgrid_type = 2;
  } else {
    PRINT("cannot handle this grid yet %s aborting\n", rf->grid->eq);
    exit(1);
  }
  
  for(i=ist+1;i<iend;i++) {
    
    if (1 == rgrid_type) {
      // dr = (b*r+a)*(b*r+a)/a; // ((r+a)/n)**2 *(n/a) -> (r+a)**2/(a*n)
      r = grid_at(rf->grid, i);
      dr = (r+rf->grid->a)*(r+rf->grid->a)/(rf->grid->a*rf->grid->n);
    } else {
      edi = exp(rf->grid->d*(double)i);
      r = rf->grid->a*(edi - 1.0);
      dr = rf->grid->a*rf->grid->d*edi;
    }
    
    zcore += rf->f[i]*r*dr;
    
  }
  zcore *= -sqrt(4.0*M_PI);
  
  
  *cm = zcore*atom.zeta;
  //printf("zcore griglia gpaw = %f\n", zcore*atom.zeta);
  //exit(0);
  
  
  
  
}

void pawxml_get_v_zero(double *v_zero, const double *grid, int n) {
  
  eval_radial_function(&atom.zero_potential, grid, v_zero, n);
  
}


// key is a number that tells if the requested wf is all electron, smooth or projector
// wave is output, 
void pawxml_get_state_wave(double *wave, const double *grid, int n, int enn, int ell, int key) {
  
  atomic_state *state;
  radial_function *rf;

  state = get_state(enn, ell);
  
  //determine radial function 
  switch (key) {  
  case AE_WAVE:
    rf = &state->ae_wave;
    break;
  case PSEUDO_WAVE:
    rf = &state->pseudo_wave;
    break;
  case PROJ_WAVE:
    rf = &state->projector_function;
    break;
  default:
    PRINT("cannot match the key in pawxml_get_state_wave, aborting...\n");
    exit(1);
    break;
  }
  
  eval_radial_function(rf, grid, wave, n);
}



static void pawxml_reader_parse(char *fbuf, int fsize) {

  int i;
  XML_Parser p;
  int done;
  
  
  // create parser
  
  p = XML_ParserCreate(NULL);
  if (! p) {
    fprintf(stderr, "Couldn't allocate memory for parser\n");
    exit(-1);
  }
  
  XML_SetElementHandler(p, start, end);
  XML_SetCommentHandler(p, comment_handler);
  XML_SetCharacterDataHandler(p, handle_data);
  
  
  // initialize the atom struct as all zeros (needed!)
  memset(&atom, 0, sizeof(paw_atom));


  done = 1;
  trace.depth = 0;
  
  if (XML_Parse(p, fbuf, fsize, done) == XML_STATUS_ERROR) {
      fprintf(stderr, "Parse error at line %" XML_FMT_INT_MOD "u:\n%s\n",
              XML_GetCurrentLineNumber(p),
              XML_ErrorString(XML_GetErrorCode(p)));
      exit(-1);
  }
  
  //cleanup parser and fbuf
  XML_ParserFree(p);
}


void pawxml_get_grid(double *a, int *istart, int *iend, int *n) {
  *a = atom.grid[0].a;
  *istart = atom.grid[0].istart;
  *iend = atom.grid[0].iend;
  *n = atom.grid[0].n;
  
}


void pawxml_get_xc_type(char *xc_type) {
  int i;
  int len;
  
  len = strlen(atom.xc_func_type);
  
  for(i=0;i<len;i++) {
    xc_type[i] = atom.xc_func_type[i];
    if(i==29) break;
  }
  
  for(i=len;i<32;i++) {
   xc_type[i] = ' '; 
  }   
}

void pawxml_get_xc_name(char *xc_name) {  
  int i;
  int len;
  
  len = strlen(atom.xc_func_name);
  
  for(i=0;i<len;i++) {
    xc_name[i] = atom.xc_func_name[i];
    if(i==29) break;
  }
  
  for(i=len;i<32;i++) {
   xc_name[i] = ' '; 
  }
  
}


void pawxml_reader_read(char *xml_fname) {
  int fname_len;
  FILE *fin;
  int fsize;
  int i;
  char *fbuf;
  
#ifndef NOMPI
  MPI_Comm_rank(MPI_COMM_WORLD, &myrank);
#endif
  
  if(!myrank) { // master process
    // handle white space padded fortran string
    fname_len = strlen(xml_fname);
    for(i=0;i<fname_len;i++) {
      if(xml_fname[i] == ' ') {
	xml_fname[i] = 0;
	break;
      }  
    }
    
    // open and read file into fbuf
    fsize = file_size(xml_fname);
    fin = open_file(xml_fname);
    fbuf = malloc(fsize);
    fread(fbuf, 1, fsize, fin);
    fclose(fin);

  }
    
#ifndef NOMPI
  MPI_Bcast(&fsize, 1, MPI_INT, 0, MPI_COMM_WORLD);
  
  if(myrank) {
    fbuf = malloc(fsize);
  }
  
  MPI_Bcast(fbuf, fsize, MPI_CHAR, 0, MPI_COMM_WORLD);
#endif

  pawxml_reader_parse(fbuf, fsize);

  if(!myrank) {
    show_atom();
  }
  
  free(fbuf);
}


#ifdef PAWXML_MAIN
int main(int argc, char *argv[]) {
  
  pawxml_reader_read(argv[1]);
  
  show_atom();

  //cleanup atom data
  pawxml_reader_cleanup();
  
  return 0;
}
#endif
