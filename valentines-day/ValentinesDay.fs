module ValentinesDay

type Approval = | Yes | No | Maybe

type Cuisine = | Korean | Turkish

type Genre = | Crime | Horror | Romance | Thriller

type Activity = | BoardGame 
                | Chill 
                | Movie of Genre 
                | Restaurant of Cuisine 
                | Walk of int

let rateActivity (activity: Activity): Approval = 
    match activity with 
    | Movie Romance | Restaurant Korean -> Approval.Yes
    | Walk walk when walk < 3           -> Approval.Yes
    | Restaurant Turkish                -> Approval.Maybe 
    | Walk walk when walk < 5           -> Approval.Maybe
    | _                                 -> Approval.No
