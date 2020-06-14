@inline function zip_unrolled(expected, them::Vararg{Any})
    map(first, them), zip_unrolled(expected, map(tail, them)...)...
end
@inline function zip_unrolled(expected, them::Vararg{Tuple{}})
    ()
end
@inline function zip_unrolled(expected, them::Vararg{Any,0})
    ntuple((@inline function (thing)
        ()
    end), expected)
end


@inline flatten_unrolled(first_one, rest...) = first_one..., flatten_unrolled(rest...)...
@inline flatten_unrolled() = ()

@inline function take_tuple(items, model)
    kept, trashed = take_tuple(tail(items), tail(model))
    (first(items), kept...), trashed
end
@inline function take_tuple(items, ::Tuple{})
    (), items
end

@inline function partition_models(items, first_model, more_models...)
    kept, trashed = take_tuple(items, first_model)
    kept, partition_models(trashed, more_models...)...
end
@inline partition_models(items) = ()

@inline function add(first_one, rest...)
    +(first_one, rest...)
end
@inline function add()
    0.0
end
