package TestEvalCat

model TestEvalCat1
Real[3] OUT;
Real[4] 'der_Q' = {0,0,0,0};
equation
  OUT = cat(1, cat(2, {{1}}, {{2}}, {{3}}, {{4}}), cat(2, {{5}}, {{6}}, {{7}}, {{8}}), cat(2, {{9}}, {{10}}, {{11}}, {{12}})) * 'der_Q';
end TestEvalCat1;

end TestEvalCat;