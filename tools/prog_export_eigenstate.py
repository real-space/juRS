#!/use/bin/env python


#---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0
#--
#-- prog_export_eigenstate.py
#--


#---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0
#--
def export_eigenstate( filename, incol=1, iband=1, ispin=1, ikpoint=1, output_quantity="density", xcrysden=False ):


    #--
    #-- Import module
    #--
    import os.path
    import mod_binary_header
    import mod_import_wavefunction
    import numpy
    import mod_export_data_xcrysden


    #--
    #-- Set parameter
    #--
    PROMPT        = __name__.upper() + " >>>"
    BOHR2ANGSTROM = 0.529177


    #--
    #-- Display argument
    #--
    print( "%s filename        = %s" % ( PROMPT, filename )         )
    print( "%s incol           = %d" % ( PROMPT, incol )            )
    print( "%s iband           = %d" % ( PROMPT, iband )            )
    print( "%s ispin           = %d" % ( PROMPT, ispin )            )
    print( "%s ikpoint         = %d" % ( PROMPT, ikpoint )          )
    print( "%s output_quantity = %s" % ( PROMPT, output_quantity )  )
    print( "%s xcrysden        = %s" % ( PROMPT, xcrysden )         )


    #--
    #-- Check existence of the file
    #--
    if not os.path.isfile( filename ):
        raise Exception( "%s ERROR: input file not found. filename = %s" % ( PROMPT, filename ) )


    #--
    #-- Import header information
    #--
    try:
        header = mod_binary_header.binary_header( filename )
    except Exception, err:
        raise Exception( err )


    #--
    #-- Check header information
    #--
    if header.get_quantity() != "wavefunctions":
        raise Exception( "%s ERROR: input file is not wavefunction file. header.quantity = %s" % ( PROMPT, header.get_quantity() ) )


    #--
    #-- Import wavefunction
    #--
    print( "%s Import wavefunction" % ( PROMPT ) )
    try:
        ( eigenenergy, occupation, wavefunction ) = mod_import_wavefunction.import_wavefunction( filename )
    except Exception, err:
        raise Exception( err )
    eigenenergy  = eigenenergy[iband-1,ispin-1,ikpoint-1]
    occupation   = occupation[iband-1,ispin-1,ikpoint-1]
    wavefunction = wavefunction[:,:,:,incol-1,iband-1,ispin-1,ikpoint-1]


    #--
    #-- Form data
    #--
    print( "%s Form data" % ( PROMPT ) )
    basename = os.path.splitext( os.path.basename( filename ) )[0]
    if output_quantity == "density":
        data = numpy.absolute( wavefunction ) ** 2
    elif output_quantity == "realpart":
        data = wavefunction.real
    else:
        raise Exception( "%s ERROR: invalid variable. output_quantity = %s" % ( PROMPT, output_quantity ) )
    lx = ( header.get_cell_x() / header.get_ngps_x() * ( header.get_ngps_x() - 1 ) ) * BOHR2ANGSTROM
    ly = ( header.get_cell_y() / header.get_ngps_y() * ( header.get_ngps_y() - 1 ) ) * BOHR2ANGSTROM
    lz = ( header.get_cell_z() / header.get_ngps_z() * ( header.get_ngps_z() - 1 ) ) * BOHR2ANGSTROM
    ox = ( 0.5 * header.get_cell_x() / header.get_ngps_x() - 0.5 * header.get_cell_x() ) * BOHR2ANGSTROM
    oy = ( 0.5 * header.get_cell_y() / header.get_ngps_y() - 0.5 * header.get_cell_y() ) * BOHR2ANGSTROM
    oz = ( 0.5 * header.get_cell_z() / header.get_ngps_z() - 0.5 * header.get_cell_z() ) * BOHR2ANGSTROM
    comment  = header.get_quantity()


    #--
    #-- Export data in xcrysden format
    #--
    if xcrysden:
        print( "%s Export data in xcrysden format" % ( PROMPT ) )
        str = basename + "_%1.1d_%4.4d_%1.1d_%3.3d_%s" % ( incol, iband, ispin, ikpoint, output_quantity ) + ".xsf"
        try:
            mod_export_data_xcrysden.export_data_xcrysden( data, str, lx, ly, lz, ox, oy, oz, comment )
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

    parser.add_argument( "--incol",   type=int, dest="incol",   default=1, help="wave function index for non-colinear spin. Default: 1" )
    parser.add_argument( "--iband",   type=int, dest="iband",   default=1, help="wave function index for band. Default: 1" )
    parser.add_argument( "--ispin",   type=int, dest="ispin",   default=1, help="wave function index for colinear spin. Default: 1" )
    parser.add_argument( "--ikpoint", type=int, dest="ikpoint", default=1, help="wave function index for k point. Default: 1" )

    parser.add_argument( "--output_quantity", type=str, dest="output_quantity", default="density", \
                         choices=[ "density", "realpart" ], \
                         help="quantity to be output. Default: density" )

    parser.add_argument( "--xcrysden", dest="xcrysden", action='store_true', default=False, \
                         help="if output in xcrysden format or not. Default: False" )


    #--
    #-- Parse command option
    #--
    args = parser.parse_args()
    print( "%s args.filename        = %s" % ( PROMPT, args.filename        ) )
    print( "%s args.incol           = %d" % ( PROMPT, args.incol           ) )
    print( "%s args.iband           = %d" % ( PROMPT, args.iband           ) )
    print( "%s args.ispin           = %d" % ( PROMPT, args.ispin           ) )
    print( "%s args.ikpoint         = %d" % ( PROMPT, args.ikpoint         ) )
    print( "%s args.output_quantity = %s" % ( PROMPT, args.output_quantity ) )
    print( "%s args.xcrysden        = %s" % ( PROMPT, args.xcrysden        ) )


    #--
    #-- Export an eigen state
    #--
    try:
        ierr = export_eigenstate( args.filename, args.incol, args.iband, args.ispin, args.ikpoint, args.output_quantity, args.xcrysden )
    except Exception, err :
        print( err )
        sys.exit( 1 )


    sys.exit( 0 )


#--
#---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0
