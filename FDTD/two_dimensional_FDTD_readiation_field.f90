program two_dimensional_FDTD_radiation_field
    ! 计算区域为(x, y):((-160, 160), (-160,160))
    ! 线电流源的TM波辐射场，场源： cos(2*pi*0.2*10*1e9*t)，位于(160,160)
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
    delta_x = 0.0177 ! 0.0187
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
    do i = 0, step_x
        do j = 0, step_y
            Ez(i, j, 0) = 0
            Hx(i, j, 0) = 0
            Hy(i, j, 0) = 0
        end do
    end do
    ! 计算10000步时间步长 分10次计算完毕
    do n = 0, 0
        ! 初始化初值 把上一次计算的结果记为初值
        do i = 0, step_x
            do j = 0, step_y
                Ez(i, j, 0) = Ez(i, j, step_t)
                Hx(i, j, 0) = Hx(i, j, step_t)
                Hy(i, j, 0) = Hy(i, j, step_t)
            end do
        end do
        ! 第n+1次FTDT求值
        do t = 1, step_t
            !先求非截断边界处的Ez
            do i = 1, step_x - 1
                do j = 1, step_y - 1
                    Ez(i, j, t) = (ca * Ez(i, j, t - 1)) + &
                    (cb * ( Hy(i, j, t - 1) - Hy(i - 1, j, t - 1) ) / delta_x) - &
                    (cb * ( Hx(i, j, t - 1) - Hx(i, j - 1, t - 1) ) / delta_y)
                    ! 如果元胞包含了电流源
                    if (i == (step_x / 2) .and. j == (step_y / 2)) then
                        Ez(i, j, t) = Ez(i, j, t) - cb * current_source((t + 0.5) * delta_t) / (delta_x * delta_y)
                    end if
                end do
            end do
            ! 边界的处理，采用二阶Mur吸收边界
            ! 先处理非角顶点 两本书有出入 这里采用不含t - 2时刻的公式 delta_x与delta_y相差不大，看作相等也没关系
            ! 处理左边界
            do j = 1, step_y - 1
                Ez(0, j, t) = Ez(1, j, t - 1) + &
                (c * delta_t - delta_x) * ( Ez(1, j, t) - Ez(0, j, t - 1) ) / (c * delta_t + delta_x) + &
                (c * c * mu_0 * delta_t) / ((c * delta_t + delta_x)) * &
                ( Hx(1, j, t - 1) + Hx(0, j, t - 1) - Hx(1, j - 1, t - 1) - Hx(0, j - 1, t - 1) ) 
            end do
            ! 处理右边界
            do j = 1, step_y - 1
                Ez(step_x, j, t) = Ez(step_x - 1, j, t - 1) + &
                (c * delta_t - delta_x) * ( Ez(step_x - 1, j, t) - Ez(step_x, j, t - 1) ) / (c * delta_t + delta_x) - &
                (c * c * mu_0 * delta_t) / ((c * delta_t + delta_x)) * &
                ( Hx(step_x - 1, j, t - 1) + Hx(step_x, j, t - 1) - Hx(step_x, j - 1, t - 1) - Hx(step_x - 1, j - 1, t - 1) ) 
            end do
            ! 处理上边界
            do i = 1, step_x - 1
                Ez(i, step_y, t) = Ez(i, step_y - 1, t - 1) + &
                (c * delta_t - delta_x) * ( Ez(i, step_y - 1, t) - Ez(i, step_y, t - 1) ) / (c * delta_t + delta_x) - &
                (c * c * mu_0 * delta_t) / ((c * delta_t + delta_y)) * &
                ( Hy(i, step_y, t - 1) + Hy(i, step_y - 1, t - 1) - Hy(i - 1, step_y, t - 1) - Hy(i - 1, step_y - 1, t - 1) ) 
            end do
            ! 处理下边界
            do i = 1, step_x - 1
                Ez(i, 0, t) = Ez(i, 1, t - 1) + &
                (c * delta_t - delta_x) * ( Ez(i, 1, t) - Ez(i, 0, t - 1) ) / (c * delta_t + delta_x) + &
                (c* c * mu_0 * delta_t) / ((c * delta_t + delta_y)) * &
                ( Hy(i, 1, t - 1) + Hy(i, 0, t - 1) - Hy(i - 1, 0, t - 1) - Hy(i - 1, 0, t - 1) ) 
            end do
            ! 处理4个角顶点 这里由于delta_x与delta_y近似相等，所以用那个公式感觉也没啥问题
            Ez(0, 0, t) = Ez(1, 1, t - 1) + (c * delta_t - sqrt(2.0) * delta_y) / (c * delta_t + sqrt(2.0) * delta_y) * &
            ( Ez(1, 1, t) - Ez(0, 0, t - 1) )

            Ez(0, step_y, t) = Ez(1, step_y - 1, t - 1) + &
            (c * delta_t - sqrt(2.0) * delta_y) / (c * delta_t + sqrt(2.0) * delta_y) * &
            ( Ez(1, step_y - 1, t) - Ez(0, step_y, t - 1) )

            Ez(step_x, 0, t) = Ez(step_x - 1, 1, t - 1) + &
            (c * delta_t - sqrt(2.0) * delta_y) / (c * delta_t + sqrt(2.0) * delta_y) * &
            ( Ez(step_x - 1, 1, t) - Ez(step_x, 0, t - 1) )

            Ez(step_x, step_y, t) = Ez(step_x - 1, step_y - 1, t - 1) + &
            ((c * delta_t - sqrt(2.0) * delta_y) / (c * delta_t + sqrt(2.0) * delta_y)) * &
            ( Ez(step_x - 1, step_y - 1, t) - Ez(step_x, step_y, t - 1) )

            ! 求Hx分量
            do i = 0, step_x
                do j = 0, step_y - 1
                    Hx(i, j, t) = Hx(i, j, t - 1) - &
                    cq * ( Ez(i, j + 1, t) - Ez(i, j, t) ) / delta_y
                end do
            end do
            ! 求Hy分量
            do i = 0, step_x - 1
                do j = 0, step_y
                    Hy(i, j, t) = Hy(i, j, t - 1) + &
                    cq * ( Ez(i + 1, j, t) - Ez(i, j, t) ) / delta_x
                end do
            end do
        end do
    end do


    open(3,file='show_lectric_field_z_step_t_10_x_0.dat')
    open(4,file='show_lectric_field_z_step_t_100_x_0.dat')
    open(5,file='show_lectric_field_z_step_t_300_x_0.dat')
    open(6,file='show_lectric_field_z_b_t.dat')
    open(7,file='show_lectric_field_z_0_0_t.dat')
    
    do j = 160, step_y
        write(3, *)(j - 160) * delta_y, Ez(160, j, 10)
        write(4, *)(j - 160) * delta_y, Ez(160, j, 100)
        write(5, *)(j - 160) * delta_y, Ez(160, j, 300)
    end do

             
    do t = 0, 800
        write(6, *)t, abs(Ez(160, 320, t))
        write(7, *)t, Ez(160, 160, t)
    end do  



end program two_dimensional_FDTD_radiation_field

!电流源
function current_source(t)
    real pi, current_source, t
    pi = 3.14
    current_source = 0.1 * cos( 2 * pi * 2 * 1e8 * t )
    return
end