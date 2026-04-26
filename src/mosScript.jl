"""
  Defines function available in the OpenModelica Scripting environment
  The functions defined here work slightly different compared to the functions defined in the main API.
  Here, most manipulate global variables which is not the case when using functions such as translate model etc
  in the main API.
"""
function loadFile(filename::String)
  try

    return true
  catch
    return false
  end
end

"""
Loads a library at the supplie dlibrary path.
"""
function loadLibrary(libraryName::String; path)
  try

    return true
  catch
    return false
  end
end


function instantiateModel(modelName::String)
  try

  catch
    return false
  end
end


function checkModel(modelName::String)
  try
    @error "checkModel API function is not yet implemented"
    return false
  catch
    return false
  end
end
