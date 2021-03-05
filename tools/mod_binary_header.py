#!/use/bin/env python


#---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0
#--
#-- mod_binary_header.py
#--
#-- 2015.06.12
#--
## @brief Python class binary_header containing binary format information
#  @author Shigeru Tsukamoto (s.tsukamoto@fz-juelich.de)
#  @date   2015.06.12
class binary_header:


    #-- Headerlength in byte
    __HEADERLENGTH = 1024
    __HEADERSYMBOL = "#_juRS_binary_datafile"
    #--                Keyword        name         num. args
    __KEYSANDNARGS = { "file:"     :( "file",      1 ),
                       "quantity:" :( "quantity",  1 ),
                       "ngps:"     :( "ngps",      4 ),
                       "cell:"     :( "cell",      3 ),
                       "bands:"    :( "bands",     1 ),
                       "spin:"     :( "spin",      1 ),
                       "kpoints:"  :( "kpoints",   1 ),
                       "date&time:":( "date&time", 2 )
                     }


    #----1---------2---------3---------4---------5---------6---------7---------8---------9---------0
    #--
    ## @brief Constructor of binary_header cleass
    #  @param[in] filename  name of the binary file to be read
    def __init__( self, filename ):


        #-- Import module
        import os.path


        #-- Set parameter
        PROMPT = __name__.upper() + " >>>"


        #-- Display argument
        print( "%s filename = %s" % ( PROMPT, filename ) )


        #-- Check existence of the file
        if not os.path.isfile( filename ):
            raise Exception( "%s ERROR: input file not found. filename = %s" % ( PROMPT, filename ) )


        #-- Open file and read the first HEADERLENGTH bytes
        with open( filename, "rb" ) as fp:
            buf = fp.read( binary_header.__HEADERLENGTH )


        #-- Split buf by white spaces
        bufs = buf.split()
        if bufs[0] != binary_header.__HEADERSYMBOL:
            raise Exception( "%s ERROR: input file is not juRS binary file" % ( PROMPT ) )


        #-- Scan bufs and find key & value
        #-- Construct header class
        i = 0
        while i < len( bufs ):
            if bufs[i] in binary_header.__KEYSANDNARGS.keys():
                ( key, nval )  = binary_header.__KEYSANDNARGS[ bufs[i] ]
                i += 1
                self.__dict__[ key ] = bufs[i:i+nval]
                print "%s %s\t=\t%s" % ( PROMPT, key, self.__dict__[ key ] )
                i += nval
            else:
                i += 1


        return


    #----1---------2---------3---------4---------5---------6---------7---------8---------9---------0
    #--
    ## @brief  Function to get quantity without quatation marks
    #  @return a string
    def get_quantity( self ):
        return self.quantity[0].replace( '"', "")


    #----1---------2---------3---------4---------5---------6---------7---------8---------9---------0
    #--
    ## @brief Function to get number of grid points in x axis in integer
    #  @return a integer
    def get_ngps_x( self ):
        return int( self.ngps[0] )


    #----1---------2---------3---------4---------5---------6---------7---------8---------9---------0
    #--
    ## @brief Function to get number of grid points in y axis in integer
    #  @return a integer
    def get_ngps_y( self ):
        return int( self.ngps[1] )


    #----1---------2---------3---------4---------5---------6---------7---------8---------9---------0
    #--
    ## @brief Function to get number of grid points in z axis in integer
    #  @return a integer
    def get_ngps_z( self ):
        return int( self.ngps[2] )


    #----1---------2---------3---------4---------5---------6---------7---------8---------9---------0
    #--
    ## @brief Function to get number of spin degree of freedom
    #  @return a integer
    def get_ngps_s( self ):
        return int( self.ngps[3] )


    #----1---------2---------3---------4---------5---------6---------7---------8---------9---------0
    #--
    ## @brief Function to get number of total grid points in x, y, z, and spin space
    #  @return a integer
    def get_ngps( self ):
        return int( self.ngps[0] * self.ngps[1] * self.ngps[2] * self.ngps[3] )


    #----1---------2---------3---------4---------5---------6---------7---------8---------9---------0
    #--
    ## @brief Function to get computation cell length in x direction
    #  @return a double
    def get_cell_x( self ):
        return float( self.cell[0] )


    #----1---------2---------3---------4---------5---------6---------7---------8---------9---------0
    #--
    ## @brief Function to get computation cell length in y direction
    #  @return a double
    def get_cell_y( self ):
        return float( self.cell[1] )


    #----1---------2---------3---------4---------5---------6---------7---------8---------9---------0
    #--
    ## @brief Function to get computation cell length in z direction
    #  @return a double
    def get_cell_z( self ):
        return float( self.cell[2] )


    #----1---------2---------3---------4---------5---------6---------7---------8---------9---------0
    #--
    ## @brief Function to get computation cell volume
    #  @return a double
    def get_cell( self ):
        return float( self.cell[0] * self.cell[1] * self.cell[2] )


    #----1---------2---------3---------4---------5---------6---------7---------8---------9---------0
    #--
    ## @brief Function to get number of bands
    #  @return a integer
    def get_bands( self ):
        return int( self.bands[0] )


    #----1---------2---------3---------4---------5---------6---------7---------8---------9---------0
    #--
    ## @brief Function to get index number of spin polarized computation
    #  @return a integer
    def get_spin( self ):
        return int( self.spin[0] )


    #----1---------2---------3---------4---------5---------6---------7---------8---------9---------0
    #--
    ## @brief Function to get index number of k-points
    #  @return a integer
    def get_kpoints( self ):
        return int( self.kpoints[0] )


#--
#---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0


#---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0
#--
#-- Main routine for test
#--
if __name__ == "__main__":

    import sys

    filename = sys.argv[1]

    test = binary_header( filename )
    print test.__dict__

    sys.exit( 0 )

#--
#---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0
