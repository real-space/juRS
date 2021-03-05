#!/use/bin/env python


#---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0
#--
#-- import_wavefunction()
#--
## @brief Python function to import wave function data from juRS output
#  @author Shigeru Tsukamoto (s.tsukamoto@fz-juelich.de)
#  @param[in] filename string: name of file to be imported. File must be wfs format from juRS
#  @return    ( eigenenergy, occupation, wavefunction )
#  @retval    eigenenergy  numpy array (nbnd,nspn,nkpt): eigen energies
#  @retval    occupation   numpy array (nbnd,nspn,nkpt): occupation numbers
#  @retval    wavefunction numpy array (nx,ny,nz,nx,nbnd,nspn,nkpt): wave functions
def import_wavefunction( filename ):


    #--
    #-- Import module
    #--
    import os.path
    import mod_binary_header
    import struct
    import numpy


    #--
    #-- Set parameter
    #--
    PROMPT = __name__.upper() + " >>>"
    __HEADERBODYSPACER = 8  #-- Spacer between header and body in byte




    #--
    #-- Display argument
    #--
    print( "%s filename = %s" % ( PROMPT, filename ) )


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
    #-- Import pairs ( eigenenergy, occupation )
    #--
    offset     = header._binary_header__HEADERLENGTH + __HEADERBODYSPACER
    datalength = header.get_bands() * header.get_spin() * header.get_kpoints() * 2 * 8
    print( "%s import ( eigenenergy, occupation ): offset     = %d" % ( PROMPT, offset )     )
    print( "%s import ( eigenenergy, occupation ): datalength = %d" % ( PROMPT, datalength ) )
    with open( filename, "rb" ) as fp:
        fp.seek( offset, 0 )
        buf = fp.read( datalength )


    #--
    #-- Unpack pairs ( eigenenergy, occupation )
    #--
    numitems = 2 * header.get_bands() * header.get_spin() * header.get_kpoints()
    fmt = "%dd" % numitems
    shape = ( header.get_bands(), header.get_spin(), header.get_kpoints() )
    print( "%s import ( eigenenergy, occupation ): numitems = %d" % ( PROMPT, numitems ) )
    print( "%s import ( eigenenergy, occupation ): fmt      = %s" % ( PROMPT, fmt )      )
    print( "%s import ( eigenenergy, occupation ): shape    = %s" % ( PROMPT, shape )    )
    buf = struct.unpack( fmt, buf )
    eigenenergy = numpy.reshape( buf[0::2], shape, order="F" )
    occupation  = numpy.reshape( buf[1::2], shape, order="F" )


    #--
    #-- Check filesize and datatype
    #-- Import wavefunctions
    #--
    filesize   = os.path.getsize( filename )
    offset     = header._binary_header__HEADERLENGTH + __HEADERBODYSPACER              \
               + header.get_bands() * header.get_spin() * header.get_kpoints() * 2 * 8
    datalength = filesize - offset
    numitems   = header.get_ngps() * header.get_bands() * header.get_spin() * header.get_kpoints()
    print( "%s import wavefunction: filesize   = %d" % ( PROMPT, filesize )   )
    print( "%s import wavefunction: offset     = %d" % ( PROMPT, offset )     )
    print( "%s import wavefunction: datalength = %d" % ( PROMPT, datalength ) )
    print( "%s import wavefunction: numitems   = %d" % ( PROMPT, numitems )   )
    if datalength % numitems != 0:
        raise Exception( "%s ERROR: datalength does not match to the number of items" % ( PROMPT ) )
    elif datalength / numitems == 8:
        datatype = numpy.float64
        print( "%s import wavefunction: datatype   = numpy.float64" % ( PROMPT ) )
    elif datalength / numitems == 16:
        datatype = numpy.complex128
        print( "%s import wavefunction: datatype   = numpy.complex128" % ( PROMPT ) )
    else:
        raise Expection( "%s ERROR: datatype cannot be determined." % ( PROMPT ) )
    with open( filename, "rb" ) as fp:
        fp.seek( offset, 0 )
        buf = numpy.fromfile( fp, dtype=datatype, count=numitems )


    #--
    #-- Reshape wavefunction data
    #--
    shape = ( header.get_ngps_x(), header.get_ngps_y(), header.get_ngps_z(), header.get_ngps_s(), header.get_bands(), header.get_spin(), header.get_kpoints() )
    wavefunction = numpy.reshape( buf, shape, order="F" )


    return ( eigenenergy, occupation, wavefunction )


#--
#---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0


#---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0
#--
#-- Test program
#--
if __name__ == "__main__":

    import sys
    import numpy

    filename = sys.argv[1]

    ( a, b, c ) = import_wavefunction( filename )
    print a[:,0,0]
    print b[:,0,0]
    print c.shape

    sys.exit( 0 )

#--
#---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0
