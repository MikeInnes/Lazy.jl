# TODO: Implement liblazy functions over standard arrays / other collections

walk(inner, outer, xs::Array) = @>> xs map(inner) outer
