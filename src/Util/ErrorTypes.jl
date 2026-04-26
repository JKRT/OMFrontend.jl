module ErrorTypes

using MetaModelica
#= ExportAll is not good practice but it makes it so that we do not have to write export after each function :( =#
using ExportAll
import ..Gettext

@UniontypeDecl Severity
@UniontypeDecl MessageType
@UniontypeDecl Message
@UniontypeDecl TotalMessage

#= severity of message =#
@Uniontype Severity begin
  @Record INTERNAL begin
  end
  @Record ERROR begin
  end
  @Record WARNING begin
  end
  @Record NOTIFICATION begin
  end
end

#= runtime scripting /interpretation error =#
@Uniontype MessageType begin
  @Record SYNTAX begin
  end
  @Record GRAMMAR begin
  end
  @Record TRANSLATION begin
  end
  @Record SYMBOLIC begin
  end
  @Record SIMULATION begin
  end
  @Record SCRIPTING begin
  end
end

const ErrorID = ModelicaInteger  #= Unique error id. Used to
look up message string and type and severity =#

@Uniontype Message begin
  @Record MESSAGE begin
    id::ErrorID
    ty::MessageType
    severity::Severity
    message::Gettext.TranslatableContent
  end
end

@Uniontype TotalMessage begin
  @Record TOTALMESSAGE begin
    msg::Message
    info::SourceInfo
  end
end

const MessageTokens = List

@exportAll()

end # ErrorTypes
