#= Compatability layer=#

module Gettext

function gettext(arg)
  return arg
end

function notrans(arg)
  return arg
end


TranslatableContent = String

export gettext
export notrans
export TranslatableContent

end
