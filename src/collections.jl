# TODO: Implement liblazy functions over standard arrays / other collections

walk(inner, outer, xs::Array) = @>> xs map(inner) outer

function Base.split{T}(xs::Vector{T}, x; keep = true)
  result = Vector{T}[]
  push!(result, T[])
  for i = 1:length(xs)
    if xs[i] == x
      (keep || !(isempty(result[end]) || i == length(xs))) &&
        push!(result, T[])
    else
      push!(result[end], xs[i])
    end
  end
  return result
end

# TODO: frequencies
