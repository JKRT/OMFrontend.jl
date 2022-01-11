
package Modelica 

package SIunits
    type Time = Real(final quantity = "Time", final unit = "s");
end SIunits;

end Modelica;

package MWE
  import SI = Modelica.SIunits;

  model A
     SI.Time t = time;
  end A;

end MWE;
