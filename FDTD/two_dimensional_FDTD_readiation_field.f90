program two_dimensional_FDTD_radiation_field
    ! 计算区域为(x, y):((-160, 160), (-160,160))
    ! 线电流源的TM波辐射场，场源： cos(2*pi*0.2*10*1e9*t)，位于(0,0)
    ! 元胞delta_x = 0.0187 delta_y = 0.0177，时间间隔delta_t = min(delta_x,delta_y)/1.5c
    ! 边界采用二阶Mur吸收边界
    real Ez(0:320, 0:320, 0:1000), Hx(0:320, 0:320, 0:1000), Hy(0:320, 0:320, 0:1000) !(x,y,t)
    real pi, mu_0, epsilon_0, c, delta_x, delta_y, delta_t, ca, cb, cp, cq
    integer step_x, step_y, step_t, i, j, t
    ! 系数
    pi = 3.14
    mu_0 = 4 * pi * 1.0e-7
    epsilon_0 = 8.85 * 1.0e-12
    c = 3.0e8
    delta_x = 0.0187
    delta_y = 0.0177
    delta_t = delta_y / (1.5 * c)
    ca = 1.0
    cb = delta_t / epsilon_0
    cp = 1.0
    cq = delta_t / mu_0
    ! 元胞数
    step_x = 320
    step_y = 320
    step_t = 1000
    ! 初始化
    do n = 0, step_t
        do i = 0, step_x
            do j = 0, step_y
                Ez(i, j, n) = 0
                Hx(i, j, n) = 0
                Hy(i, j, n) = 0
            end do
        end do
    end do
    ! FTDT求值
    do t = 1, step_t
        !先求Ez分量
        do i = 1, step_x - 1
            do j = 1, step_y - 1
                Ez(i, j, t) = ca * Ez(i, j, t - 1) + &
                cb * ( Hy(i, j, t - 1) - Hy(i - 1, j, t - 1) ) / delta_x - &
                cb * ( Hx(i, j, t - 1) - Hx(i, j - 1, t - 1) ) / delta_y
                ! 如果元胞包含了电流源
                if (i == (step_x / 2) .and. j == (step_y / 2)) then
                    Ez(i, j, t) = Ez(i, j, t) - cb * current_source((t + 0.5) * delta_t) / (delta_x * delta_y)
                end if
            end do
        end do
        ! 边界的处理，采用二阶Mur吸收边界
        ! 先处理非角顶点 两本书有出入 要自己推一下
        ! 处理左边界
        do j = 1, step_y - 1
            Ez(0, j, t) = Ez(1, j, t - 1) + &
            (c * delta_t - delta_x) * ( Ez(1, j, t) - Ez(0, j, t - 1) ) / (c * delta_t + delta_x) - &
            (c* c * mu_0 * delta_t) / (2.0 * (c * delta_t + delta_y)) * &
            ( Hx(0, j, t - 1) - Hx(0, j - 1, t - 1) + Hx(1, j, t - 1) - Hx(1, j - 1, t - 1) ) 
        end do
        ! 处理右边界
        do j = 1, step_y - 1
            Ez(step_x, j, t) = Ez(step_x - 1, j, t - 1) + &
            (c * delta_t - delta_x) * ( Ez(step_x - 1, j, t) - Ez(step_x, j, t - 1) ) / (c * delta_t + delta_x) - &
            (c* c * mu_0 * delta_t) / (2.0 * (c * delta_t + delta_y)) * &
            ( Hx(step_x, j, t - 1) - Hx(step_x, j - 1, t - 1) + Hx(step_x - 1, j, t - 1) - Hx(step_x - 1, j - 1, t - 1) ) 
        end do
        ! 处理上边界
        do i = 1, step_x - 1
            Ez(i, step_y, t) = Ez(i, step_y - 1, t - 1) + &
            (c * delta_t - delta_x) * ( Ez(i, step_y - 1, t) - Ez(i, step_y, t - 1) ) / (c * delta_t + delta_x) - &
            (c* c * mu_0 * delta_t) / (2.0 * (c * delta_t + delta_y)) * &
            ( Hx(i, step_y, t - 1) - Hx(i, step_y - 1, t - 1) + Hx(i - 1, step_y, t - 1) - Hx(i - 1, step_y - 1, t - 1) ) 
        end do
        ! 处理下边界
        do i = 1, step_x - 1
            Ez(i, 0, t) = Ez(i, 1, t - 1) + &
            (c * delta_t - delta_x) * ( Ez(i, 1, t) - Ez(i, 0, t - 1) ) / (c * delta_t + delta_x) - &
            (c* c * mu_0 * delta_t) / (2.0 * (c * delta_t + delta_y)) * &
            ( Hx(i, 0, t - 1) - Hx(i, 1, t - 1) + Hx(i - 1, 0, t - 1) - Hx(i - 1, 1, t - 1) ) 
        end do
        ! 处理4个角顶点 这里由于delta_x与delta_y近似相等，所以用那个公式感觉也没啥问题
        Ez(0, 0, t) = Ez(1, 1, t - 1) + (c * delta_t - sqrt(2.0) * delta_y) / (c * delta_t + sqrt(2.0) * delta_y) * &
        ( Ez(1, 1, t) - Ez(0, 0, t - 1) )

        Ez(0, step_y, t) = Ez(1, step_y - 1, t - 1) + (c * delta_t - sqrt(2.0) * delta_y) / (c * delta_t + sqrt(2.0) * delta_y) * &
        ( Ez(1, step_y - 1, t) - Ez(0, step_y, t - 1) )

        Ez(step_x, 0, t) = Ez(step_x - 1, 1, t - 1) + (c * delta_t - sqrt(2.0) * delta_y) / (c * delta_t + sqrt(2.0) * delta_y) * &
        ( Ez(step_x - 1, 1, t) - Ez(step_x, 0, t - 1) )

        Ez(step_x, step_y, t) = Ez(step_x - 1, step_y - 1, t - 1) + &
        ((c * delta_t - sqrt(2.0) * delta_y) / (c * delta_t + sqrt(2.0) * delta_y)) * &
        ( Ez(step_x - 1, step_y - 1, t) - Ez(step_x, step_y, t - 1) )

        ! 求Hx Hy分量
        do i = 0, step_x - 1
            do j = 1, step_y - 1
                Hx(i, j, t) = cp * Hx(i, j, t - 1) - &
                cq * ( Ez(i, j + 1, t) - Ez(i, j, t) ) / delta_y
                Hy(i, j, t) = cp * Hy(i, j, t - 1) + &
                cq * ( Ez(i + 1, j, t) - Ez(i, j, t) ) / delta_x
            end do
        end do
    end do

    open(3,file='electric_field_z_while_step_t_1000.dat')
    
    do j = 0, step_y
        write(3, *)j, Ez(0, j, 1000)
    end do       
    



end program two_dimensional_FDTD_radiation_field

!电流源
function current_source(t)
    real pi, current_source, t
    pi = 3.14
    current_source = cos( 2 * pi * 2 * 1e8 * t )
    return
end