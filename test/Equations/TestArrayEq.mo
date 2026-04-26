model TestArrayEq
parameter Real[3] 'B1.ex_a' = {1., 2., 3.};
Real[2] 'B1.f_c'(each unit = "N", each quantity = "Force");
Real[3] 'B1.frame_a.f'(each unit = "N", each quantity = "Force");
parameter Real[3] 'B1.ey_a'(each unit = "1") = {1,2,3};
equation
'B1.frame_a.f' = cat(2, {{0.0}, {1.0}, {0.0}}, {{0.0}, {0.0}, {1.0}}) * 'B1.f_c';
end TestArrayEq;