#ifdef DEBUG_ALL
#define DEBUG
#endif

!!#define DEBUG
! #define FULL_DEBUG

! This file will be preprocessed twice, first
! with R1_C2 == 1, and then with R1_C2 == 2
! then the two parts are concatenated to one source.
! This requires that the module head has to be
! appearing, when R1_C2 == 1 and the line "end module <name>"
! with R1_C2 == 2.
#ifdef R1_C2
#if R1_C2 == 1
! module head

!! @author Paul Baumeister, Andrea Nobile
!! @version 3.0
!!
!! the module force computes the PAW forces.
!!
!! The method chosen here is to derive the
!! non-local projectors rather than the wave
!! functions itself. The background for this
!! is the scaling behaviour. The number of
!! grid points treated in a process will be kept
!! constant even with growing system size because
!! one can parallelize more. The number of bands
!! in each process will grow with the system size.
!! Therefore, the projectors are derived instead
!! of the wave functions.

! Andrea Nobile 4.2014 + 5.2014 tested different variations on what to derive
! A.N. 5.2014 fixed bug when k-points > 1 on correction hamiltonian (complex case) 
module forces
  use configuration, only: o ! output unit, 0: no output
implicit none
  private ! default for this module namespace

  public :: force_calculation
  public :: write_force_file
  public :: derive_id
#ifdef EXTENDED
  public :: test
#endif

  interface force_calculation
    module procedure force_r, force_c
  endinterface

  character(len=*), parameter, private :: fun=': ', sym = 'FRC' !! module symbol

  contains

  !! write the atoms as Z, position, force to a file
  integer function write_force_file( project, a, g, Etot ) result( ist )
  use configuration, only: MAX_INPUTFILE_LEN => MaxInputFileNameLen
  use configuration, only: FRC_FILEEXTENSION => FRC_FileNameExtension
  use configuration, only: COMMENTCHARs
  use type_atom, only: atom
  use type_grid, only: grid, BC_PERIODIC
    character(len=*), intent(in)          :: project        !! project name
    type(atom), intent(in)                :: a(:)           !! list of *all* atoms
    type(grid), intent(in), optional      :: g              !! grid descriptor
    real, intent(in), optional            :: Etot           !! total energy

#ifdef DEBUG
    character(len=*), parameter           :: fun = ' write_force_file: '
#endif
    integer, parameter                    :: unt = 9
    character(len=MAX_INPUTFILE_LEN+4)    :: filename
    integer                               :: na, ia, id
    real                                  :: cell(3,3)

    write(unit=filename,fmt='(9A)',iostat=ist) trim(project), FRC_FILEEXTENSION
#ifdef DEBUG
    if( ist /= 0 .and. o>0) write(o,'(9A)') sym, fun, 'failed to generate <projectname>', FRC_FILEEXTENSION
#endif
    if( ist /= 0 ) return

    open(unit=unt,file=filename,iostat=ist,action='write')
#ifdef DEBUG
    if( ist /= 0 .and. o>0) write(o,'(9A)') sym, fun, 'failed to open "', trim(filename), '" for writing.'
#endif
    if( ist /= 0 ) return
    na = size(a,1)
    write(unt,fmt='(I6,A)')         na, ' atoms' ! number of atoms as in jmol-files
    do ia = 1, na ! loop over all atoms
      write(unt,fmt='(I3, 3F14.6, A6, 3F18.10)') a(ia)%s%iZ, a(ia)%pos, ' ', a(ia)%frc
    enddo ! ia
    write(unt,fmt='(9A)')           COMMENTCHARs(1), ' Z, Pos(aB), Frc(Ha/aB)'
    write(unt,fmt='(9A)')           COMMENTCHARs(1), ' Project: ', trim(project)
    if( present( g ) ) then
      write(unt,fmt='(9A)')         COMMENTCHARs(1), ' UnitCell(aB)'
      cell = 0.
      do id = 1, 3
        cell(id,id) = g%s(id)
      enddo ! id
      do id = 1, 3
        write(unt,fmt='(A,3F12.6)') COMMENTCHARs(1), cell(:,id)
      enddo ! id
      write(unt,fmt='(2A,3L2)')     COMMENTCHARs(1), ' Periodic', ( g%bc(1:3,1) == BC_PERIODIC )
    endif ! present g
    if( present( etot ) ) &
      write(unt,fmt='(2A,F22.12)')   COMMENTCHARs(1), ' TotalEnergy(Ha)', etot
    close(unit=unt,iostat=ist)

#ifdef DEBUG
    if(o>0) write(o,'(9A)') sym, fun, 'forces written to "', trim(filename), '".'
#endif
  endfunction ! write_force_file

! end of the head part
#endif


#if R1_C2 == 1
#define REAPLEX real
  !! computes the forces on each atom
  function force_r &
#else
#define REAPLEX complex
  !! see documentation of force_calculation_r
  function force_c &
#endif
                          ( global, &
                            gc, gd, ge, &!nmesh, &
                            a, &
                            atm, &
                            spsi, &
                            psi_state, &
                            vTOT, &
                            vBAR, &
                            rhov, &
                            ves, &
                            kpt, &
                            interpolationo, &
                            meshrefinement, &
                            directions ) result( avgfrc )
  !
  ! calculation of the forces acting on the atoms
  ! according to the Hellmann-Feynman theorem.
  !
  use configuration, only: WARNING
  use configuration, only: o
#ifdef FULL_DEBUG
  use constants, only: Y00 => ONESQRTFOURPI
  use toolbox, only: radial_hist
#endif
  use constants, only: Pi
  use type_species, only: species, I_PRJ
  use type_info, only: info
  use type_state, only: state
  use type_atom, only: atom, ITP_DEF, MSH_DEF
  use type_grid, only: grid, BC_FINITE, BC_PERIODIC, NUMBER_OF_PERIODIC_IMAGES
  use type_bfun, only: add_s_projector
  use MPItools, only: MPIreduce0
  use type_comp, only: comp, comp_set, project
  use type_proj, only: proj, proj_set, proj_free
  use type_proj, only: prjct => project
  use interpolation, only: interpolate
  use type_kpoint, only: kpoint
  use type_grid, only: periodic_positions
  use type_species, only: I_RHOC, I_SMT, I_VBAR, I_TRU, I_VES!, I_R
  use operators, only: bloch_phase
  
#ifdef CONSTR
  use constraints, only: i_constraints
#endif
  use MPItools, only: MPIallsum, operator(.MPIsum.), MPIbarrier
  use MPIconst, only: Wtime
#ifndef NOMPI
  use MPIconst, only: PREC
!   use mpi, only: MPI_Bcast
#endif
  use atomicomm, only: AtomReduce
    type(info), intent(in)                :: global      !! info object
    type(grid), intent(in)                :: gc          !! coarse grid descriptor
    type(grid), intent(in)                :: gd          !! dense grid descriptor
    type(grid), intent(in)                :: ge          !! potential grid descriptor
    type(atom), intent(inout)             :: a(:)           !! parallelized list of atoms
    type(atom), intent(inout)             :: atm(:)         !! list of *all* atoms
    REAPLEX, intent(in)                   :: spsi(:,:)   !! smooth wave functions [on gc]
    type(state), intent(in)               :: psi_state(:) !! state object for each state
    real, intent(in)                      :: vTOT(:,:,:,:)     !! local effective potential [on ge]
    real, intent(in)                      :: vBAR(:,:,:,:)     !! local potential correction [on ge]
    real, intent(in)                      :: rhov(:,:,:,:)      !! valence density [on gd]
    real, intent(in)                      :: ves(:,:,:)         !! Electrostatic pot. [on ge]
    type(kpoint), intent(in)              :: kpt(:)     !! list of kpoints
    integer, intent(in), optional         :: interpolationO ! half order of double grid interpolator
    integer, intent(in), optional         :: meshrefinement ! as many times denser double grid
    logical, intent(in), optional         :: directions(1:3) !! calculate only in those direction, default = true
    real                                  :: avgfrc(1:3) !! result: average force

    character(len=*), parameter           :: FM  = '(2A,A6,3A,I6,F10.6,ES10.2E2)'
#ifdef DEBUG
    integer                               :: u = 0 ! output unit
#else
    integer, parameter                    :: u = 0 ! no output
#endif
    real, allocatable                     :: rhov_e(:,:,:,:) ! (ge%ng,ns)
    real, allocatable                     :: duhlm(:,:) ! ((ellmax+1)**2,na)
    real, allocatable                     :: duhlmtmp(:) ! ((ellmax+1)**2)
    real, allocatable                     :: drhoc(:,:,:,:) ! (gd%ng,ns)
    real, allocatable                     :: dvbar(:,:,:,:) ! (gd%ng,ns)
    type(proj), allocatable               :: dprj(:) ! (na)
    REAPLEX                               :: dcpsi(2*gc%naprj,size(spsi,2)) ! (2*naprj,nbsk)
    integer                               :: ia, ja, io
    real                                  :: origins(3,27)
    integer                               :: np, ip, npmax, naprj, mmlnm, mlm2
    complex                               :: phase
    integer                               :: nbsk, ibsk
    integer                               :: nspn, ispn
    integer                               :: nbnd, ibnd
    integer                               :: nkpt, ikpt
    real                                  :: f ! temp
    integer                               :: ell, emm, ilm
    integer                               :: i1, i2, i3
    real                                  :: occ, ene, HmES
    real                                  :: cdc
    integer                               :: ist, ii, is, ellmax
    real                                  :: origin(3)

    real                                  :: spinfactor, rmx
    integer                               :: nn, ni, ns
    real, allocatable                     :: imagepos(:,:)

    real, allocatable                     :: dves(:,:,:) ! derived electrostatic potential
    real                                  :: uhlm1(81)
#ifdef CONSTR
    integer                               :: im
#endif


    real, allocatable                     :: fi(:,:,:)

    integer                               :: ipos(3), me, iq, ir
    real                                  :: pos(3)
    real                                  :: wgt(3,0:1), tmp, wtt

    integer                               :: id, ierr, itp, msh
    logical                               :: dirs(3)
    real                                  :: t(0:9) ! time
    dirs = .true. ; if( present( directions ) ) dirs = directions

#ifdef DEBUG
    u = o ! set unit u to the output unit
#endif
    if(u>0) write(u,'(9A)') sym, fun, 'start'

    ! defaults from module type_atom
    itp = ITP_DEF ; if( present( interpolationo ) ) itp = max( 0, interpolationO )
    msh = MSH_DEF ; if( present( meshrefinement ) ) msh = max( 1, meshrefinement )
    if(u>0) write(u,'(3A/,3A,9(I3,A))') &
      sym, fun, 'double grid method for projectors:', &
      sym, fun, '    ', 2*itp, '-th order,', msh, 'x denser grid.'

    itp = 0
    msh = 1

    ! more atoms in the parallelized list than atoms in the system ! error
    if( size(a) > size(atm) ) stop 'FRC: list of atoms and list of all atoms are interchanged!'

    ! force calculation

    ! for any radially given function f, the gradient operator goes as
    !
    ! f(\vec{r}) = f_{\ell}(r) r^\ell Y_{\ell m}(\hat{r})
    !
    ! \dnach{  f(\vec{r}) }{ \vec{r} } =
    !   f_{\ell}(r) * \dnach{  r^\ell Y_{\ell m}(\hat{r}) }{ \vec{r} }
    ! + \dnach{   f_{\ell}(r) }{ r } \frac{ \vec{r} }{ r } r^\ell Y_{\ell m}(\hat{r})
    !

    t(0) = Wtime() ! start


    allocate( fi(1:4,3,0:size(atm)), stat=ist ) ; if( ist /= 0 ) stop 'FRC allocation of FI failed!'
    fi = 0. ! force ingredients

    ! for the derived local electrostatic potential
    allocate( dves(ge%ng(1),ge%ng(2),ge%ng(3)), stat=ist ) ; if( ist /= 0 ) stop 'FRC allocation of dVES failed!'
    ! derive the electrostatic potential w.r.t to r(1:3) by finite differences

    ns = size(rhov,4)
    allocate( rhov_e(ge%ng(1),ge%ng(2),ge%ng(3),ns), stat=ist ) ; if( ist /= 0 ) stop 'FRC allocation of RHOV_e failed!'

    ii = interpolate( rhov, rhov_e, gd, ge, mscale=ge%ng(1)/gd%ng(1) )
    if( ns /= size(vBAR,4) ) stop 'FRC vBAR should have the same spin dim as RHOV!'


    if(u>0) write(u,'(3A,3(3A,L2))') sym, fun, 'Directions', ( '  ',achar(119+id),':',dirs(id),id=1,3 )

    do id = 1, 3 ! loop over spatial direction x, y, z

      avgfrc(id) = 0. ! init result

      if( .not. dirs(id) ) then
        if(u>0) write(u,'(5A,I4,F10.6,ES10.2)') sym, fun, 'skip  ', achar(119+id), '-direction.'
        cycle ! skip this direction
      endif ! not in this direction

      if(u>0) write(u,'(5A,I4,F10.6,ES10.2)') sym, fun, 'start ', achar(119+id), '-direction.'


      t(1) = Wtime() ! start derivation

      if(u>0) write(u,'(9A)') sym, fun, 'derive ves'
      ! derive the electrostatic potential w.r.t to r(1:3) by finite differences
      
      ist = derive_id( ge, id, ves, dves )

      t(2) = Wtime() ! start direction
#define DERIVE_VES
#ifdef DERIVE_VES
      do ia = 1, size(a)
        ! project into the temporary variable uhlm1
        call project( cmp=a(ia)%cmp, Ves=dves, c=uhlm1, dV=ge%hvol )
        ellmax = 2*a(ia)%s%ellmax
        mlm2 = (ellmax+1)**2
        f = sum( a(ia)%qlm(1:mlm2) * uhlm1(1:mlm2) )
        fi(1,id,a(ia)%ja) = f 
      enddo ! ia
#else
      do ia = 1, size(a)
        ! project into the temporary variable uhlm1
        call project( cmp=a(ia)%dcmp(id), Ves=ves, c=uhlm1, dV=ge%hvol )
        ellmax = 2*a(ia)%s%ellmax
        mlm2 = (ellmax+1)**2
        f = sum( a(ia)%qlm(1:mlm2) * uhlm1(1:mlm2) )
        fi(1,id,a(ia)%ja) = -f 
      enddo ! ia
#endif

      ! communication for the compensation charges times derived electrostatic potential
      call MPIallsum( fi(1,id,:), ge%comm )

      t(3) = Wtime() ! electrostatic projection done


      allocate( dprj(size(a)), stat=ist ) ; if( ist /= 0 ) stop 'FRC allocation of dPRJ failed!'

      naprj = 0
      mmlnm = 0 ! could be zero, too
      do ia = 1, size(a)
        naprj = naprj + a(ia)%s%mlnm
        mmlnm = max( mmlnm, a(ia)%s%mlnm )

        ! for all periodic images of that atoms, projector
        ! parts my reach into the spatial domain of this process
        dprj(ia) = proj_set( bf=a(ia)%s%fprj, origins=a(ia)%imagepos(1:3,1:a(ia)%nimages), &
                            offset=gc%off, iphase=a(ia)%prjphase, &
                            hg=gc%h, ng=gc%ng(1:3), &
!                            interpolation=itp, meshrefinement=msh, &
                            interpolation=1, meshrefinement=1, &
                            derive2i=id ) ! derived w.r.t. rv(id)
        !! Warning: the projectors are derived w.r.t their argument (r-R),
        !! the derivative w.r.t. the atomic origin R is of opposite sign
        !!
        !! dprj = d/dr prj( r-R ) = -d/dR prj( r-R )
      enddo ! ia

      call MPIbarrier( gc%comm )

      t(4) = Wtime() ! setup of derived projectors done

      !===============================================================================
      !===============================================================================
      !===============================================================================

      nkpt = global%nkpt
      nspn = global%nspn
      nbnd = global%nbnd

      nbsk = nkpt * nspn * nbnd
#ifdef DEBUG
      if( nbsk /= size( spsi, 2 ) ) stop 'force_calculation: number of wave functions is wrong.'
#endif

!       allocate( dcpsi(2*naprj,nbsk), ctm(mmlnm) ) ! mmlnm == maximum of all a(:)%s%mlnm

      ! double convention: the projection coefficients in dcpsi are stored
      ! 2*io(1)+1 : 2*io(1)+np(1), 2*io(1)+np(1)+1 : io(1)+2*np(1), 2*io(2)+1 :....
      !      a(1)%dc                             a(1)%c                       a(2)%dc
      dcpsi = 0. ! init
      ! === loop over all KS-states ===================================
      ibsk = 0
      do ikpt = 1, nkpt
        do ispn = 1, nspn
          do ibnd = 1, nbnd
            ibsk = ibsk+1

            do ia = 1, size(a)

              np = a(ia)%s%mlnm
              io = a(ia)%offprj
              ! project dc = <d/dR p|f>
              call prjct( p=dprj(ia), f=spsi(:,ibsk), c=dcpsi(2*io+1:2*io+np,ibsk), &
                          dV=gc%hvol, eikL=kpt(ikpt)%eik )
              ! project c = <p|f>
              call prjct( p=a(ia)%prj, f=spsi(:,ibsk), c=dcpsi(2*io+np+1:2*io+2*np,ibsk), &
                          dV=gc%hvol, eikL=kpt(ikpt)%eik )

            enddo ! ia

          enddo ! ibnd
        enddo ! ispn
      enddo ! ikpt
      ! === end loop over all KS-states ===============================
      call proj_free( dprj ) ; deallocate( dprj )
#ifdef NaN_SEARCH
      if( any( dcpsi /= dcpsi ) ) stop 'FRC: NaN in dcpsi (before MPIreduce0)!'
#endif

      ! transfer the projection coefficients to the atom owners
      do ia = 1, size(a)
        np = a(ia)%s%mlnm
        io = a(ia)%offprj
        ! communication collected ! receiving process is the atom owner
!         call MPIreduce0( dcpsi(2*io+1:2*io+2*np,:), a(ia)%comm ) ! copy in+out
        call AtomReduce( dcpsi(2*io+1:2*io+2*np,:), gc%comm, a(ia)%owner, a(ia)%plist )
      enddo ! ia

#ifdef NaN_SEARCH
      if( any( dcpsi /= dcpsi ) ) stop 'FRC: NaN in dcpsi (after MPIreduce0)!'
#endif



      t(5) = Wtime() ! projection done

      ! === setup of the derived atomic density matrix =========================
      do ia = 1, size(a)
        if( a(ia)%owner /= gc%rank ) cycle ! atom owner only
        np = a(ia)%s%mlnm
        io = a(ia)%offprj
        ! owner task

#ifdef CONSTR
        im = i_constraints( a(ia)%ja ) ! index of the constraint mask
        if(o>0) write(o,'(3A,9(I0,A))') sym, fun, 'atom #', a(ia)%ja, ' uses occupation numbers of mask #', im
#endif
        f = 0. ! init temp.
        ! === loop over all KS-states ===================================
        do ibsk = 1, nbsk
          occ = psi_state(ibsk)%occ * psi_state(ibsk)%wgt
#ifdef CONSTR
          occ = psi_state(ibsk)%cocc( im ) * psi_state(ibsk)%wgt ! use special occupation numbers
#endif
          if( occ > 0. ) then
            is   = psi_state(ibsk)%jspn ! overall spin index
            Ene  = psi_state(ibsk)%ene ! energy of the Kohn-Sham state

            HmES = 0. ! init
            do i1 = 1, np
              do i2 = 1, np
                ! set up derived density matrix elements
#if R1_C2 == 1
                cdc = ( dcpsi(2*io+i2,ibsk) * dcpsi(2*io+np+i1,ibsk) + &
                        dcpsi(2*io+np+i2,ibsk) * dcpsi(2*io+i1,ibsk) )
#else
                !cdc = real( conjg(dcpsi(2*io+i1,ibsk) * dcpsi(2*io+np+i2,ibsk) ) + &
                !                  dcpsi(2*io+np+i1,ibsk) * dcpsi(2*io+i2,ibsk)   )
                cdc = real( conjg(dcpsi(2*io+i1,ibsk)) * (dcpsi(2*io+np+i2,ibsk) ) + &
                                  conjg(dcpsi(2*io+np+i1,ibsk)) * dcpsi(2*io+i2,ibsk)   )
#endif
                HmES = HmES + ( a(ia)%Hm(i2,i1,is) - Ene * a(ia)%s%overlap_matrix(i2,i1) ) * cdc
                

              enddo ! i2
            enddo ! i1
            f = f + occ * HmES
          endif ! occ > 0.
        enddo ! ibsk
        ! === end loop over all KS-states ===============================

        ! sum over parallelized states: (only atom owners use the equi_comm here)
        f = f .MPIsum. gc%equi_comm
        fi(3,id,a(ia)%ja) = f ! store

      enddo ! ia
      ! === end setup of the derived atomic density matrix ==============


      ! distribute information from the atom owners among all
      do ja = 1, size(atm)
#ifdef DEBUG
        do ia = 1, size(a)
          if( a(ia)%ja /= ja ) cycle
          if( a(ia)%owner /= atm(ja)%owner ) stop 'FRC ERROR: atom-ownership inconsistency detected!'
        enddo ! ia
#endif

#ifndef NOMPI
        call MPI_Bcast( fi(3,id,ja), 1, PREC(1), atm(ja)%owner, gc%comm, ierr )
#endif
        f = fi(3,id,ja)
        if(u>0) write(u,FM) sym, fun, 'atomic', ': id=', achar(119+id), ', ja=', ja, f, f
      enddo ! ja


      t(6) = Wtime() ! redistribution done



      ! for any radially given function f, the gradient operator reads:
      !
      ! f(\vec{r}) = f_{\ell}(r) r^\ell Y_{\ell m}(\hat{r})
      !
      ! \dnach{  f(\vec{r}) }{ \vec{r} } =
      !   f_{\ell}(r) * \dnach{  r^\ell Y_{\ell m}(\hat{r}) }{ \vec{r} }
      ! + \dnach{   f_{\ell}(r) }{ r } \frac{ \vec{r} }{ r } r^\ell Y_{\ell m}(\hat{r})
      !
      ! vBAR and the smooth core density are spherically
      ! symmetric. Therefore Y00 (ilm=1) is the corresponding
      ! spherical harmonic function.


      allocate( dvBAR(ge%ng(1),ge%ng(2),ge%ng(3),ns), stat=ist ) ; if( ist /= 0 ) stop 'FRC allocation of dVBAR failed!'
#define DERIVE_VBAR
#ifdef DERIVE_VBAR  

      do ia = 1, size(a)
        dvBAR = 0. ! clear
        do is = 1, 1 ! vBAR is usually spin-independent, so spare the spins in the setup

          call add_s_projector( a(ia)%s%vbar, &
                                qnt=dvBAR(:,:,:,is), &
                                hg=ge%h, origins=a(ia)%imagepos(1:3,1:a(ia)%nimages), &
                                offset=ge%off, &
                                derive2i=id )
        enddo ! is
        if( ns > 1 ) dvBAR(:,:,:,2) = dvBAR(:,:,:,1) ! copy
        ! now integrate with the the valence density on the dense grid
        f = ( sum(dvBAR*rhov_e)*ge%hvol )
        fi(4,id,a(ia)%ja) = f ! store

      enddo ! ia
#else
      ist = derive_id( ge, id, rhov_e(:,:,:,1), dves )

       do ia = 1, size(a)
        dvBAR = 0. ! clear
        do is = 1, 1 ! vBAR is usually spin-independent, so spare the spins in the setup

          call add_s_projector( a(ia)%s%vbar, &
                                qnt=dvBAR(:,:,:,is), &
                                hg=ge%h, origins=a(ia)%imagepos(1:3,1:a(ia)%nimages), &
                                offset=ge%off, &
                                derive2i=0 )
        enddo ! is
        if( ns > 1 ) dvBAR(:,:,:,2) = dvBAR(:,:,:,1) ! copy
        ! now integrate with the the valence density on the dense grid
        f = ( sum(dvBAR(:,:,:,1)*dves)*ge%hvol )
        fi(4,id,a(ia)%ja) = -f ! store

      enddo ! ia
#endif

      deallocate( dvBAR )

      ! communication for the correction potential vBAR
      call MPIallsum( fi(4,id,:), ge%comm )


#ifdef DEBUG
      if( u > 0 ) then
        do ja = 1, size(atm)
          f = fi(4,id,ja)
          if(u>0) write(u,FM) sym, fun, 'vbar', ': id=', achar(119+id), ', ja=', ja, f, f
        enddo ! ja
        do ja = 1, size(atm)
          f = fi(1,id,ja)
          if(u>0) write(u,FM) sym, fun,  'cmp', ': id=', achar(119+id), ', ja=', ja, f, f
        enddo ! ja
      endif ! display
#endif


      t(7) = Wtime() ! potential correction done

      ! Caution:
      ! the core density region may be larger than the sphere.
      ! therefore, the list of all atoms atm(:) has to be used here
      ns = size(vTOT,4)
      allocate( drhoc(ge%ng(1),ge%ng(2),ge%ng(3),ns), stat=ist ) ; if( ist /= 0 ) stop 'FRC allocation of dRHOC failed!'

      spinfactor = 1.0 ; if( ns > 1 ) spinfactor = 0.5


      rmx = minval(gc%s) ! the smallest extend of the supercell

      do ja = 1, size(atm) ! loop list of all atoms
        drhoc = 0. ! init
        ! how many periodic image positions are required?
        nn = ceiling( atm(ja)%s%rhoc%rcut/rmx )

        allocate( imagepos(3,(nn+1+nn)**3), stat=ist ) ; if( ist /= 0 ) stop 'FRC allocation of IMAGEPOS failed!'
        ! if the core density is extended further than rmx,
        ! generate up to 125 image positions for 3 periodic boundary conditions
        ni = periodic_positions( ge, atm(ja)%relpos, imagepos, number_of_images=nn )
        do is = 1, ns
          call add_s_projector( atm(ja)%s%rhoc, & ! bfun
                                qnt=drhoc(:,:,:,is), &
                                hg=ge%h, &
                                origins=imagepos(:,1:ni), &
                                offset=ge%off, &
                                factor=spinfactor, &
                                derive2i=id )
        enddo ! is
       
        
      
        deallocate( imagepos )
        ! now integrate with the effective potential [on ge]
        f = ( sum(drhoc*(vTOT))*ge%hvol ) .MPIsum. ge%comm
        !f = ( sum(drhoc*(vTOT - vBAR))*ge%hvol ) .MPIsum. ge%comm
        if(u>0) write(u,FM) sym, fun, 'core', ': id=', achar(119+id), ', ja=', ja, f, f
        fi(2,id,ja) = f
      enddo ! ja

      deallocate( drhoc )


      t(8) = Wtime() ! smooth core density done


      do ja = 1, size(atm)
        ! according to C.Rostgaard
        f = -fi(1,id,ja)+fi(2,id,ja)+fi(3,id,ja)+fi(4,id,ja)
        atm(ja)%frc(id) = f
        if(u>0) write(u,FM) sym, fun, 'total', ': id=', achar(119+id), ', ja=', ja, f, f
      enddo ! ja

      ! result
      f = sum( atm(:)%frc(id) )
      avgfrc(id) = f/max(size(atm),1)
      if(u>0) write(u,FM) sym, fun, 'total', ': id=', achar(119+id), ', all',  0, f, f

      ! subtract the average force
      do ja = 1, size(atm)
        atm(ja)%frc(id) = atm(ja)%frc(id) - avgfrc(id)
      enddo ! ja
#ifdef DEBUG
#ifdef TIME
      if(o>0) write(o,'(5A,7F10.3,9A)') sym, fun, 'times for id=', achar(119+id), '      ', t(2:8)-t(1:7), ' sec'
#endif
#endif
    enddo ! id

    t(9) = Wtime() ! total time
#ifdef TIME
    if(o>0) write(o,'(3A,F10.3,9A)') sym, fun, 'total time', t(9)-t(0), ' sec'
#endif
    deallocate( rhov_e, dves )

  endfunction ! force_calculation


#if R1_C2 == 2
! module tail

#define FIRST_DERIV_FORCE_ORDER

  integer function derive_id( g0, id, v, dv ) result( ist )
  use type_grid, only: grid
  use boundary, only: dataexchange
    type(grid), intent(in)    :: g0 !! grid descriptor
    integer, intent(in)       :: id !! direction to derive to
    real, intent(in)          :: v(:,:,:) !! array to be derived
    real, intent(out)         :: dv(:,:,:) !! derived array

    type(grid)                :: g
    integer                   :: nf, ii, ix, iy, iz
    real, allocatable         :: c(:), vec(:,:,:)
#ifdef DEBUG
    character(len=*), parameter :: fun = ' derive_id: '

    selectcase( id )
    case( 1:3 ) ! ok
    case default ; stop 'FRC derive_id: id must be in {1,2,3}!'
    endselect ! id
#endif
    dv = 0. ! init

    g = g0 ! copy
#ifdef DEBUG
    if( any( shape( v) /= g%ng(1:3) ) ) stop 'FRC derive_id: a dim of V does not match g%NG'
    if( any( shape(dv) /= g%ng(1:3) ) ) stop 'FRC derive_id: a dim of DV does not match g%NG'
#endif

    nf = max( 1, min( g%nf(id), 8 ) )
#ifdef FIRST_DERIV_FORCE_ORDER
    nf = 3
#endif
    !write (*,*) 'DERIVE: using order ', nf, 'to derive '
    ! remove halos in the other 2 directions
    g%nh = 0      ; g%nf = 0
    g%nh(id) = nf ; g%nf(id) = nf
    allocate( c(-nf:nf) ) ; c = 0.
    
#ifdef DEBUG
    if(o>0) write(o,'(5A,I2,A,3I3)') sym, fun, 'derive by FD id=', achar(119+id), ' (FD-order) =', 2*Nf, ' halos =', g%nh
#endif

#ifdef FULL_DEBUG
!     if(o>0) write(o,'(5A,I2)') sym, fun, 'derive by FD id=', achar(119+id), ' (FD-order) =', 2*Nf
#endif
    selectcase( Nf )
    case(0) ; if(o>0) write(o,'(9A)') sym, fun, 'derivative in ', achar(119+id), '-direction will be zero!' ; return
    case(1) ; c(1:Nf) = (/1./) / ( 2. *g%h(id) )
    case(2) ; c(1:Nf) = (/8.,-1./) / ( 12. *g%h(id) )
    case(3) ; c(1:Nf) = (/135.,-27.,3./) / ( 180. *g%h(id) )
    case(4) ; c(1:Nf) = (/4032.,-1008.,192.,-18./) / ( 5040. *g%h(id) )
    case(5) ; c(1:Nf) = (/21000.,-6000.,1500.,-250.,20./) / ( 25200. *g%h(id) )
    case(6) ; c(1:Nf) = (/712800.,-222750.,66000.,-14850.,2160.,-150./) / ( 831600. *g%h(id) )
    case(7) ; c(1:Nf) = (/315315.,-105105.,35035.,-9555.,1911.,-245.,15./) / ( 360360. *g%h(id) )
    case(8) ; c(1:Nf) = (/640640.,-224224.,81536.,-25480.,6272.,-1120.,128.,-7./) / ( 720720. *g%h(id) )
    case default ; stop 'FRC derive_id: after initialization, NF must bin in [1,8].'
    endselect

    c(-1:-Nf:-1) = -c(1:Nf)
    if( abs(sum(c)) > 1.E-7 ) stop 'FRC derive_id: coefficients do not sum up to zero.'

#ifdef FULL_DEBUG
    if(o>0) write(o,'(5A,F10.3,9A)') sym, fun, 'for id=', achar(119+id), ' allocate ', product( g%ng(1:3)+2*g%nh )*8 /1024.**2, ' MiBytes'
#endif

    allocate( vec( 1-g%nh(1):g%ng(1)+g%nh(1), &
                   1-g%nh(2):g%ng(2)+g%nh(2), &
                   1-g%nh(3):g%ng(3)+g%nh(3) ), &
              stat=ii ) ; if( ii /= 0 ) stop 'FRC derive_id: allocation of VEC failed!'
    vec = 0. ! init
    vec(1:g%ng(1),1:g%ng(2),1:g%ng(3)) = v ! copy into the central region of vec
    call dataexchange( g, vec )

#ifdef FULL_DEBUG
!     if(o>0) write(o,'(3A,9I6)') sym, fun, 'shape of dv and vec =', shape(dv), shape(vec)
#endif

    selectcase( id )
    case( 1 ) ! derive w.r.t. x-direction
      do iz = 1, g%ng(3) ; do iy = 1, g%ng(2) ; do ix = 1, g%ng(1)
        dv(ix,iy,iz) = 0.
        do ii = -Nf, Nf
          dv(ix,iy,iz) = dv(ix,iy,iz) + vec(ix+ii,iy,iz)*c(ii)
        enddo ! ii
      enddo ; enddo ; enddo ! ixyz
    case( 2 ) ! derive w.r.t. y-direction
      do iz = 1, g%ng(3) ; do iy = 1, g%ng(2)
        dv(:,iy,iz) = 0.
        do ii = -Nf, Nf
          dv(:,iy,iz) = dv(:,iy,iz) + vec(:,iy+ii,iz)*c(ii)
        enddo ! ii
      enddo ; enddo ! iz
    case( 3 ) ! derive w.r.t. z-direction
      do iz = 1, g%ng(3)
        dv(:,:,iz) = 0.
        do ii = -Nf, Nf
          dv(:,:,iz) = dv(:,:,iz) + vec(:,:,iz+ii)*c(ii)
        enddo ! ii
      enddo ! iz
    case default ; stop 'FRC derive_id: id must be in {1,2,3}!'
    endselect ! id

    ist = 0
  endfunction ! derive_id

#ifdef EXTENDED
  integer function test( )
    write(*,*,iostat=test) __FILE__,' no module test implemented'
  endfunction ! test
#endif

endmodule ! forces
#endif
#endif
