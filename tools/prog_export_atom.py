#!/use/bin/env python


#---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0
#--
#-- prog_export_atom.py
#--


#---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0
#--
def export_atom( filename, xcrysden=False ):


    #--
    #-- Import module
    #--
    import os.path
    import mod_input_file
    import mod_atom_coordinate
    import mod_export_atom_xcrysden


    #--
    #-- Set parameter
    #--
    PROMPT = __name__.upper() + " >>>"
    ANGSTROM2BOHR = 1.0 / 0.529177
    NM2BOHR       = 1.0 / 0.0529177
    PM2BOHR       = 1.0 / 52.9177
    SCALES        = { "aB":1.0, "Ang":ANGSTROM2BOHR, "nm":NM2BOHR, "pm":PM2BOHR }


    #--
    #-- Display argument
    #--
    print( "%s filename = %s" % ( PROMPT, filename ) )
    print( "%s xcrysden = %s" % ( PROMPT, xcrysden ) )


    #--
    #-- Check existence of the file
    #--
    if not os.path.isfile( filename ):
        raise Exception( "%s ERROR: input file not found. filename = %s" % ( PROMPT, filename ) )


    #--
    #-- Import header information
    #--
    try:
        input = mod_input_file.input_file( filename )
    except Exception, err:
        raise Exception( err )


    #--
    #--
    #--
    atoms = []
    if input.exist( "atoms" ):
        if input.exist( "atom_scale" ):
            scale = SCALES[ input.get( "atom_scale" ) ]
        else:
            scale = 1.0
        for line in input.get( "atoms" ):
            name, x, y, z = line.split( None, 4 )[0:4]
            x = float( x ) * scale
            y = float( y ) * scale
            z = float( z ) * scale
            atoms.append( mod_atom_coordinate.atom_coordinate( x, y, z, name=name ) )

    elif input.exist( "atoms_fractional" ):
        if input.exist( "cell_scale" ):
            scale = SCALES[ input.get( "atom_scale" ) ]
        else:
            scale = 1.0
        ( cell_x, cell_y, cell_z ) = input.get( "cell_size" ).split()
        for line in input.get( "atoms_fractional" ):
            name, x, y, z = line.split( None, 4 )[0:4]
            x = ( float( x ) * float( cell_x ) - 0.5 * float( cell_x ) ) * scale
            y = ( float( y ) * float( cell_y ) - 0.5 * float( cell_y ) ) * scale
            z = ( float( z ) * float( cell_z ) - 0.5 * float( cell_z ) ) * scale
            atoms.append( mod_atom_coordinate.atom_coordinate( x, y, z, name=name ) )

    else:
        raise Exception( "%s ERROR: no atomic structure information is found." % ( PROMPT ) )


    #--
    #-- Display atom coordinate
    #--
    for atom in atoms:
        print( "%s name = %2s x = %12.4f y = %12.4f z = %12.4f nz = %3d" % ( PROMPT, atom.name, atom.x, atom.y, atom.z, atom.nz ) )


    #--
    #-- Export atomic structure in xcrysden format
    #--
    if xcrysden:
        print( "%s Export atomic structure in xcrysden format" % ( PROMPT ) )
        try:
            basename = os.path.splitext( os.path.basename( filename ) )[0]
            str = basename + "_geometry" + ".xsf"
            mod_export_atom_xcrysden.export_atom_xcrysden( str, atoms )
        except Exception, ierr:
            raise Exception( ierr )


    return 0

#--
#---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0


#---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0
#--
#-- Main program
#--
if __name__ == "__main__":


    import sys
    import argparse


    #--
    #-- Set parameter
    #--
    PROMPT = __name__.upper() + " >>>"


    #--
    #-- Construct parser instance
    #--
    desc = u"{0} [Options] [Args]\nDetailed options -h or --help".format(__file__)
    parser = argparse.ArgumentParser(description=desc)


    #--
    #-- Set command options
    #--
    parser.add_argument( "filename", type=str, help="file name to be read" )

    parser.add_argument( "--xcrysden", dest="xcrysden", action='store_true', default=False, \
                         help="if output in xcrysden format or not. Default: False" )


    #--
    #-- Parse command option
    #--
    args = parser.parse_args()
    print( "%s args.filename = %s" % ( PROMPT, args.filename ) )
    print( "%s args.xcrysden = %s" % ( PROMPT, args.xcrysden ) )


    #--
    #-- Export an eigen state
    #--
    try:
        ierr = export_atom( args.filename, args.xcrysden )
    except Exception, err :
        print( err )
        sys.exit( 1 )


    sys.exit( 0 )


#--
#---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0
