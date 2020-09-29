ComplexType = NFComplexType
@Uniontype NFComplexType begin
@Record COMPLEX_CLASS begin
end

@Record COMPLEX_EXTENDS_TYPE begin
  baseClass::InstNode
end

@Record COMPLEX_CONNECTOR begin
  potentials::List{InstNode}
  flows::List{InstNode}
  streams::List{InstNode}
end

@Record COMPLEX_EXPANDABLE_CONNECTOR begin
  potentiallyPresents::List{InstNode}
  expandableConnectors::List{InstNode}
end

@Record COMPLEX_RECORD begin
  constructor::InstNode
  fields::List{Field}
end

@Record COMPLEX_EXTERNAL_OBJECT begin
  constructor::InstNode
  destructor::InstNode
end
end
