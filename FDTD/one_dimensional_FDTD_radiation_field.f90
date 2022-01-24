program one_dimensional_FDTD_radiation_field
    ! 平面电流源的辐射场分布，边界采用Mur一阶近似吸收边界
    ! 场源位于z=250处，为xoy平面，源为一时谐源：cos(2*pi*3*1e8*t)，区域为真空
    real mu_0, epsilon_0, pi, c, delta_z, delta_t, ca, cb, cp, cq, alpha
    real Ex(0:500,0:500), Hy(0:500,0:500) !(z,t)
    integer step, source_z, t, z
    external current_source

    pi = 3.14
    mu_0 = 4 * pi * 1.0e-7
    epsilon_0 = 8.85 * 1.0e-12
    c = 3.0e8
    delta_z = 1.0 / 40.0
    delta_t = delta_z / (2 * c)
    step = 500
    source_z = 250
    ca = 1.0
    cb = (delta_t) / (epsilon_0 * delta_z)
    cp = 1.0
    cq = (delta_t) / (mu_0 * delta_z)
    alpha = (c * delta_t - delta_z) / (c * delta_t + delta_z)
    ! open(1,file='electric_field.dat')
    ! open(2,file='magnetic_field.dat')
    open(3,file='electric_field_while_delta_t_200.dat')
    open(4,file='magnetric_field_while_delta_t_400.dat')

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
        do z = 0, step
            if ( z == source_z ) then
                ! 处理元胞包含电流源时
                Ex(z,t) = ca * Ex(z, t - 1) - cb * ( Hy(z, t - 1) - Hy(z - 1, t - 1) ) &
                - cb * current_source( (t + 0.5) * delta_t )
            else if ( z == 0 ) then
                ! 处理边界 Mur一阶近似吸收左边界
                Ex(0, t) = Ex(1, t - 1) + alpha * ( Ex(1, t) - Ex(0, t - 1) )
            else if ( z == step ) then
                ! 处理边界 Mur一阶近似吸收右边界
                Ex(step, t) = Ex(step - 1, t - 1) + alpha * ( Ex(step - 1, t) - Ex(step, t - 1) )
            else
                Ex(z,t) = ca * Ex(z, t - 1) - cb * ( Hy(z, t - 1) - Hy(z - 1, t - 1) )
            end if
        end do
        ! Hy分量 z = 0时 Hy(z + 1/2)的计算不涉及截断边界以外的计算， Hy仅仅需计算到第step个元胞  ，因为第step + 1元胞已经超出了截断边界。Hy的划分为z+1/2
        do z = 0, step - 1
            Hy(z,t) = cp * Hy(z, t - 1) - cq * ( Ex(z + 1, t) - Ex(z, t) )
        end do

        !Hy(0, t) = Hy(1, t - 1) + alpha * ( Hy(1, t) - Hy(0, t - 1) )
        !Hy(step, t) = Hy(step - 1, t - 1) + alpha * ( Hy(step - 1, t) - Hy(step, t - 1) )
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
    ! t=200*delta_t时的Ex数据
    do z = 0, step
        write(3,*)z, Ex(z,200)
        write(4,*)z, Hy(z,400)
    end do
end program one_dimensional_FDTD_radiation_field

!电流源
function current_source(t)
    real pi, current_source, t
    pi = 3.14
    current_source = cos( 2 * pi * 3 * 1e8 *t )
    return
end