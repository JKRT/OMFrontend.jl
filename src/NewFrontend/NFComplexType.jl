@Uniontype NFComplexType
ComplexType = NFComplexType

@Record CLASS begin
end

@Record EXTENDS_TYPE begin
  baseClass::InstNode
end

@Record CONNECTOR begin
  potentials::List{InstNode}
  flows::List{InstNode}
  streams::List{InstNode}
end

@Record EXPANDABLE_CONNECTOR begin
  potentiallyPresents::List{InstNode}
  expandableConnectors::List{InstNode}
end

@Record RECORD begin
  constructor::InstNode
  fields::List{Field}
end

@Record EXTERNAL_OBJECT begin
  constructor::InstNode
  destructor::InstNode
end
end
