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
        do z = 1, step
            Ex(z,t) = Ex(z, t - 1) - cb * ( Hy(z, t - 1) - Hy(z - 1, t - 1) )
            ! 处理元胞包含电流源时
            if ( z == source_z ) then
                
                Ex(z,t) = Ex(z,t) - cb * current_source( (t + 0.5) * delta_t )                
            end if
        end do
        ! Hy分量 z = 0时 Hy(z + 1/2)的计算不涉及截断边界以外的计算
        do z = 0, step - 1
            Hy(z,t) = cp * Hy(z, t - 1) - cq * ( Ex(z + 1, t) - Ex(z, t) )
        end do
        ! 处理边界 Mur一阶近似吸收左边界
        Ex(0, t) = Ex(1, t - 1) + alpha * ( Ex(1, t) - Ex(0, t - 1) )
        ! 处理边界 Mur一阶近似吸收右边界
        Hy(step,t) = Hy(step - 1, t - 1) + alpha * ( Hy(step - 1, t) - Hy(step, t - 1) )
    end do
    
    ! 数据整理
    open(6,file='electric_field_FDTD_solution_while_step_t_400.dat')
    open(7,file='electric_field_analytic_solution_while_step_t_400.dat')
    open(8,file='compare_analytic_solution_and_FDTD_solution_while_step_t_400.dat')
    do z = 0, step
        write(6, *)z, Ex(z, 400) ! t=400*delta_t时的由FDTD解出的Ex
        temp = electric_field_analytic_solution(400.0 * delta_t, z * delta_z, c) ! t=400*delta_t时的由解析解解出的Ex
        write(7, *)z, temp
        write(8, *)z, Ex(z, 400), temp
    end do
    
end program one_dimensional_FDTD_radiation_field

!电流源
function current_source(t)
    real pi, current_source, t
    pi = 3.14
    current_source = cos(2 * pi * 3 * 1e8 * t)
    return
end

!由解析式求得的精确解
function electric_field_analytic_solution(t, z, v)
    real pi, impedance, electric_field_analytic_solution, t, z, v
    external current_source
    pi = 3.14
    impedance = 120.0 * pi !波阻抗
    electric_field_analytic_solution = (impedance / 2) * current_source(t - z / v)
    return
end