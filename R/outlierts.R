correct_outliers = function(vec, window_size=5, step_size=2, std=2, base = "mean")
{
    len = length(vec)

    if(window_size > len)
    {return(vec)}


    for (i in seq(1,len-(window_size),by = step_size))
    {
        current_bucket=vec[i:(i+window_size)]
        std_bucket = sd(current_bucket)
        if (base == "mean")
        {
            base_bucket = mean(current_bucket)
        }
        else if(base == "median")
        {
            base_bucket = median(current_bucket)
        }
        for(j in 1:length(current_bucket))
        {
            if(current_bucket[j] > (base_bucket + std_bucket*std) || current_bucket[j] < (base_bucket - std_bucket*std))
            {
                current_bucket[j] = base_bucket
            }
        }
        vec[i:(i+window_size)] = current_bucket
    }
    return(vec)
}
