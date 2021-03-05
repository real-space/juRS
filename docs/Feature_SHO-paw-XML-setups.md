**PAW Setups using analytical projector shapes**

The module all_electron (AE) is now capable of generating
data sets for the Projector Augmented Wave (PAW) method
using the analytically known eigenfunctions of the 
Spherical Harmonic Oscillator (SHO).
AE starts by obtaining the self-consistent solution of an
isolated atom in perfect spherical symmetry.
Hint: if a directory ./pot/ exists, ./pot/rV.00Z files are
generated to store the self-consistent spherical potential.
This accelerates the generation procedure after the 1st run.
The final data set is exported in the .xml format of GPAW.
AE uses a configuration string for elements which combines
  * the electronic occupation numbers for the atomic calculation
  * the numbers of projectors used in each ell-channel
  * the radii and the spread

*Occupation numbers for the atomic self-consistent calculation*
We can specify the valence occupation numbers as
    4p 3
which will set the three n=4 ell=1 states as valence states.
This implies that the 2p and 3p states are treated as core states.
If the 4p-subshell should be unoccupied it is enough to specify
    4p
in order to declare the limit of the ell=1 core levels.
The example above has an occupation of 0.5 electrons in each state.
The syntax also supports spin polarization by adding a 2nd number, e.g.
    4p 2.3 0.7
however, it is deactivated for the PAW-data generation process for now.
Several of these specifications can be concatenated.

*Numbers of projectors per ell-channel*
By specifying the valence state, e.g.
    4p
There is one projector in the ell=1-channel (p-channel).
In order to add another projector, add asterisks like
    4p*

*Radii and Spread*
Four different radii and one spread parameter have to be determined:
The first radius after | refers to that of the generalized Gaussian 
used as compensation_charge in GPAW. We can extract the values used 
in the PAW setups published in the GPAW repository by
    grep gauss *.PBE
and multiply these with sqrt(10.). Often, you will find that this value matches the smallest cutoff radius.
The second radius refers to the sphere inside which the smooth potential is a parabola.
It should be the minimum of the *rc* of each partial wave.
Inside the third radius the smooth core density differs from the true core density.
Also here, the minimum of the *rc* of each partial wave should be a good choice.
The fourth radius defines the point where the true and smooth partial waves are matched.
It should be larger than 7--10 sigma to make sure that the projectors are effectively zero there.
We can specify the spread of SHO functions after the keyword *sigma*.

**Here are some examples**
element  Be  2s* 2 2p 3d | 1.5 1.5 1.5 3.8 sigma .422
element  C   2s* 2 2p* 2 3d | 1.2 1.2 1.2 3.9 sigma .428
element  N   2s* 2 2p* 3 3d 4f | 1.0 1.0 1.0 2.5 sigma .275
element  P   3s* 2 3p* 3 3d 4f | 1.8 1.8 1.8 4.6 sigma .512
element  Fe  4s* 2 4p* 3d* 6 4f 5g | 2.0 2.0 2.0 6.8 sigma .755
element  Cu  4s* 1 4p* 3d* 10 4f 5g | 2.0 2.0 2.0 6.75 sigma .75
element  Au  6s* 1 6p* 5d* 10 5f 5g | 2.5 2.5 2.5 6.0 sigma .667

*Explained in details for the gold example*
The keyword
    element
in the juRS input file declares that an element configuration will follow. Then,
    Au
sets is the symbol that should match the atoms in the atom list. Mind that
if there is no gold in the list of atoms, the generation procedure will not
be envoked. Further
    6s* 1
defines that 1s, 2s, 3s, 4s and 5s levels are fully occupied core states.
The 6s level is a valence states and it is half-occupied.
The s-channel will feature 2 projectors, s and s*.
    6p*
defines that 2p, 3p, 4p and 5p levels are fully occupied core states.
The 6p states are unoccupied valence states.
The p-channel will feature two projectors, p and p*.
    5d* 10
defines that 3d and 4d levels are fully occupied core states.
The 5d states are fully occupied valence states.
The d-channel will feature two projectors, d and d*.
    5f
defines that the 4f levels are fully occupied core states.
The 5f states are unoccupied valence states.
The f-channel will feature one projector.
    5g
makes that an unbound solution for ell=4 will be calculated to generate one projector.
After the vertical bar
    |
radii in units of Bohr will follow. Here,
    .790569
is the radius of the generalized Gaussians used as compensation charges.
The value was extracted from gpaw-setups-0.9.9672/Au.PBE, grep for *gauss*.
    2.500
is the minimum of the rc= values of all valence_states. It is used for
the pseudization of the local potential with a parabola. The second mentioning of
    2.500
makes that the pseudization of the core charge density happens at the same radius.
    6.005
defines the radius at which the matching of the value and derivative 
of true and smooth partial wave takes place. Finally,
    sigma .667196080
makes sure that we use the right PAW generation routine for analytical SHO-type
projectors with a spread of 0.66719608 Bohr.
