module BookingUpForBeauty

// The following line is needed to use the DateTime type
open System

let schedule = DateTime.Parse    

let hasPassed = (>) DateTime.Now

let isAfternoonAppointment (appointmentDate: DateTime): bool =
    let
        time = appointmentDate.Hour 
    in
        (&&) ((<=) 12 <| time) ((>) 18 <| time)

let description (appointmentDate: DateTime): string  = 
    sprintf <| "You have an appointment on %A." <| appointmentDate

let anniversaryDate(): DateTime = 
    new DateTime(DateTime.Now.Year, 9, 15, 0, 0, 0)
