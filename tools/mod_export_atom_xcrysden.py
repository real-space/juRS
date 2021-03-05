#!/use/bin/env python


#--------1---------2---------3---------4---------5---------6---------7---------8---------9---------0
#--
#-- mod_export_atom_xcrysden.py
#--
## @brief Python function to export atomic structre in xcrysden format
#  @author Shigeru Tsukamoto (s.tsukamoto@fz-juelich.de)
#  @param[in] filename name of file to be exported
#  @param[in] atoms    a list of atom_coordinate class
def export_atom_xcrysden( filename, atoms ):


    #--
    #-- Parameter
    #--
    PROMPT        = __name__.upper() + " >>>"
    BOHR2ANGSTROM = 0.529177                  # 1 a_B = 0.529177 Ang.


    #--
    #-- Display summary
    #--
    print( "%s filename   = %s" % ( PROMPT, filename )     )
    print( "%s len(atoms) = %d" % ( PROMPT, len( atoms ) ) )


    #--
    #-- Header part
    #--
    str  = "# Generated by transcood program\n"
    str += "# Number of atoms: %4d\n" % len( atoms )
    str += "ATOMS\n"


    #--
    #-- Atomic structure part
    #--
    for atom in atoms:
        str += "%3d %12.8f %12.8f %12.8f\n" % ( atom.nz, atom.x * BOHR2ANGSTROM, atom.y * BOHR2ANGSTROM, atom.z * BOHR2ANGSTROM )


    #--
    #-- Output XCrySDen file
    #--
    with open( filename, "w" ) as fp:
        fp.write( str )


    return 0

