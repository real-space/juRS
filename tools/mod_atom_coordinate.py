#!/use/bin/env python


#---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0
#--
#-- class atom_coordinate
#--
## @brief Python class atom_coordinate containing atomic coordinate, element number, and element symbol
#  @author Shigeru Tsukamoto (s.tsukamoto@fz-juelich.de)
class atom_coordinate:


    #---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0
    #--
    #-- Define element name
    #--
    ## @brief Static list containing element symbols
    ELEMNAME = ( "H", "He", "Li", "Be", "B",  "C",  "N",  "O",  "F",  "Ne", \
                "Na", "Mg", "Al", "Si", "P",  "S",  "Cl", "Ar", "K",  "Ca", \
                "Sc", "Ti", "V",  "Cr", "Mn", "Fe", "Co", "Ni", "Cu", "Zn", \
                "Ga", "Ge", "As", "Se", "Br", "Kr", "Rb", "Sr", "Y",  "Zr", \
                "Nb", "Mo", "Tc", "Ru", "Rh", "Pd", "Ag", "Cd", "In", "Sn", \
                "Sb", "Te", "I",  "Xe", "Cs", "Ba", "La", "Ce", "Pr", "Nd", \
                "Pm", "Sm", "Eu", "Gd", "Tb", "Dy", "Ho", "Er", "Tm", "Yb", \
                "Lu", "Hf", "Ta", "W",  "Re", "Os", "Ir", "Pt", "Au", "Hg", \
                "Tl", "Pb", "Bi", "Po", "At", "Rn", "Fr", "Ra", "Ac", "Th", \
                "Pa", "U",  "Np", "Pu", "Am", "Cm", "Bk", "Cf", "Es", "Fm", \
                "Md", "No", "Lr", "Rf", "Db", "Sg", "Bh", "Hs", "Mt", "Ds",
                "Rg", "Cn" )


    #----1---------2---------3---------4---------5---------6---------7---------8---------9---------0
    #--
    ## @brief Constructor of atom_coordinate class
    #  @param[in] x    x coordinate in Bohr length
    #  @param[in] y    y coordinate in Bohr length
    #  @param[in] z    z coordinate in Bohr length
    #  @param[in] num  element number
    #  @param[in] name element symbol
    #  @note Only either num or name can be specified.
    def __init__( self, x, y, z, num=None, name=None ):


        #--
        #-- Parameter
        #--
        PROMPT = __name__.upper() + " >>>"


        #--
        #-- Check argument
        #--
        if ( num != None and name != None ) or ( num == None and name == None ):
            raise Exception( "%s ERROR: either num or name has to be specified. num = %s, name = %s" % ( PROMPT, num, name ) )


        #--
        #-- Input variables
        #--
        try:
            self.x    = float( x )
            self.y    = float( y )
            self.z    = float( z )
            if name == None:
                self.nz   = int( num )
                self.name = ELEMNAME[ int( num ) - 1 ]
            elif num == None:
                self.nz   = ELEMNAME.index( name ) + 1
                self.name = name
        except Exception:
            raise ValueError, "ValueError"


        return


    #--
    #----1---------2---------3---------4---------5---------6---------7---------8---------9---------0


#---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0
#--
#-- class atom
#--
if __name__ == "__main__":

    import sys

    try:
        atom = atom_coordinate( 1.0, 2.0, -1.0, num=8 )
        print atom.x, atom.y, atom.z, atom.nz, atom.name
        atom = atom_coordinate( 1.0, 2.0, -1.0, name="U" )
        print atom.x, atom.y, atom.z, atom.nz, atom.name
    except Exception, ierr:
        print ierr
        sys.exit( 1 )


    sys.exit( 0 )
