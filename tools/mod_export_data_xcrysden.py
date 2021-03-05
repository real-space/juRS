#!/use/bin/env python


#---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0
#--
#-- mod_export_data_xcrysden()
#--
## @brief Python function to export multiple 3D data in xcrysden format
#  @author Shigeru Tsukamoto (s.tsukamoto@fz-juelich.de)
#  @date   2014.04.09 Created
#  @date   2014.06.17 Move the positions of ox, oy, oz to the end of argumens
#  @date   2014.06.17 Change ox, oy, and oz to be optional argument
#  @date   2014.06.17 3D and 4D are both available for input
#  @param[in] a        numpy array: of 3D or 4D data
#  @param[in] filename string: name of file to be exported
#  @param[in] lx       double: length of the x axis
#  @param[in] ly       double: length of the y axis
#  @param[in] lz       double: length of the z axis
#  @param[in] ox       double: position of origin in x axis (default=0.0)
#  @param[in] oy       double: position of origin in y axis (default=0.0)
#  @param[in] oz       double: position of origin in z axis (default=0.0)
#  @param[in] comment  string: comment text to exported file
def export_data_xcrysden( a, filename, lx, ly, lz, ox=0.0, oy=0.0, oz=0.0, comment="comment" ):


    #--
    #-- Import module
    #--
    import numpy


    #--
    #-- Parameter
    #--
    PROMPT = __name__.upper() + " >>>"
    CHUNK  = 8


    #--
    #-- Display dummy argument
    #--
    print( "%s a.shape = %s"  % ( PROMPT, a.shape ) )
    print( "%s filename = %s" % ( PROMPT, filename ) )
    print( "%s lx = %f"       % ( PROMPT, lx ) )
    print( "%s ly = %f"       % ( PROMPT, ly ) )
    print( "%s lz = %f"       % ( PROMPT, lz ) )
    print( "%s ox = %f"       % ( PROMPT, ox ) )
    print( "%s oy = %f"       % ( PROMPT, oy ) )
    print( "%s oz = %f"       % ( PROMPT, oz ) )
    print( "%s comment = %s"  % ( PROMPT, comment ) )


    #--
    #-- Check array dimension ( 3 and 4 are available )
    #--
    print( "%s a.shape = %s" % ( PROMPT, a.shape ) )
    if a.ndim == 3:
        ( nx, ny, nz ) = a.shape
        nn             = 1
        a = a.reshape( ( nx, ny, nz, nn ), order="F" )
    elif a.ndim == 4:
        ( nx, ny, nz, nn ) = a.shape
    else:
        raise Exception( "ERROR: invalid array dimension" )


    #--
    #-- Format output string
    #--
    str  = "BEGIN_BLOCK_DATAGRID_3D\n"
    str += "  %s\n" % comment
    for ii in xrange( nn ):
        str += "  BEGIN_DATAGRID_3D_data%d\n" % ii
        str += "    %d  %d  %d\n" % ( nx, ny, nz )
        str += "    %f  %f  %f\n" % ( ox, oy, oz )
        str += "    %f  %f  %f\n" % ( lx, 0.0, 0.0 )
        str += "    %f  %f  %f\n" % ( 0.0, ly, 0.0 )
        str += "    %f  %f  %f\n" % ( 0.0, 0.0, lz )
        b = a[:,:,:,ii].reshape( ( nx * ny * nz, ), order="F" )
        for i, val in enumerate( b ):
            str += "  " if i % CHUNK == 0 else ""
            str += "  %13.6e" % val
            str += "\n" if i % CHUNK == CHUNK - 1 or i == len( b ) - 1 else ""

        str += "  END_DATAGRID_3D\n"

    str += "END_BLOCK_DATAGRID_3D\n"


    #--
    #-- Output the string to file
    #--
    with open( filename, "w" ) as fp:
        fp.write( str )


    return 0

#--
#---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0




#---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0
#--
if __name__ == "__main__":


    import sys, numpy, math


    a = numpy.empty( ( 10, 10, 10, 2 ), dtype=numpy.float64, order="F" )
    for m in xrange( a.shape[3] ):
        for k in xrange( a.shape[2] ):
            for j in xrange( a.shape[1] ):
                for i in xrange( a.shape[0] ):
                    a[i,j,k,m] = 10.0 / math.sqrt( ( i - 3.3 * ( m + 1 ) ) ** 2 + ( j - 3.3 * ( m + 1 ) ) ** 2 + ( k - 3.3 * ( m + 1 ) ) ** 2 )


    try:
        return_code = export_data_xcrysden( a, "test.xsf", 1.0, 1.0, 1.0 )
    except Exception, errmsg:
        sys.exit( errmsg )


#    print return_code
    sys.exit( 0 )

#--
#---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0
