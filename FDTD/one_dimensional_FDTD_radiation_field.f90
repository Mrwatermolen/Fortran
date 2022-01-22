program one_dimensional_FDTD_radiation_field
    ! 平面电流源的辐射场分布，边界采用Mur一阶近似吸收边界
    ! 场源位于z=250处，为xoy平面，源为一时谐源：cos(2*PI*3*1e8*t)，区域为真空
    real Mu0, EP0, c, deltaZ, deltaT, ca, cb, cp, cq, aplha
    real Ex(0:500,0:500), Hy(0:500,0:500) !(z,t)
    integer step, sourceZ, t, z
    external current_source

    Mu0 = 4 * PI * 1.0e-7
    EP0 = 8.85 * 1.0e-12
    c = 3.0e8
    deltaZ = 1.0 / 40.0
    deltaT = deltaZ / (2 * c)
    step = 500
    sourceZ = 250
    ca = 1.0
    cb = (deltaT) / (EP0 * deltaZ)
    cp = 1.0
    cq = (deltaT) / (Mu0 * deltaZ)
    aplha = (c * deltaT - deltaZ) / (c * deltaT + deltaZ)
    ! open(1,file='electric_field.dat')
    ! open(2,file='magnetic_field.dat')
    open(3,file='electric_field_while_deltaT_200.dat')

    ! 赋初值
    do i = 0, step
        do j = 0, step
            Ex(i,j) = 0
            Hy(i,j) = 0
        end do
    end do

    ! 主体
    do t = 1, step
        ! 先计算Ex
        do z = 1, step - 1
            if ( z == sourceZ ) then
                ! 处理元胞包含电流源时
                Ex(z,t) = ca * Ex(z, t - 1) - cb * ( Hy(z, t - 1) - Hy(z - 1, t - 1) ) &
                - cb * current_source( t * deltaT )
            else
                Ex(z,t) = ca * Ex(z, t - 1) - cb * ( Hy(z, t - 1) - Hy(z - 1, t - 1) )
            end if
        end do
        do z = 1, step - 1
            Hy(z,t) = cp * Hy(z, t - 1) - cq * ( Ex(z + 1, t) - Ex(z, t) )
        end do

        ! 处理边界 Mur一阶近似吸收边界
        Ex(0, t) = Ex(1, t - 1) + aplha * ( Ex(1, t) - Ex(0, t - 1) )
        Hy(0, t) = Hy(1, t - 1) + aplha * ( Hy(1, t) - Hy(0, t - 1) )
        Ex(step, t) = Ex(step - 1, t - 1) + aplha * ( Ex(step - 1, t) - Ex(step, t - 1) )
        Hy(step, t) = Hy(step - 1, t - 1) + aplha * ( Hy(step - 1, t) - Hy(step, t - 1) )
    end do
    
    ! 数据整理
    ! do t = 0, step
    !     write(1,*)"*************************", t, "*************************"
    !     write(2,*)"*************************", t, "*************************"
    !     do z = 0, step
    !         write(1,*)z, Ex(z,t)
    !         write(2,*)z, Hy(z,t)
    !     end do
    ! end do
    ! t=200*deltaT时的Ex数据
    do z = 0, step
        write(3,*)z, Ex(z,200)
    end do
end program one_dimensional_FDTD_radiation_field

!电流源
function current_source(t)
    real PI, current_source, t
    PI = 3.14
    current_source = cos( 2 * PI * 3 * 1e8 *t )
    return
end