
type  User  = unit

let a queryable =
    let queryable:(IQueryable<User> -> IQueryable<User>) =
        match inUse with
        |true ->
            (fun iq -> iq.Where(fun (c:CustomersEntity) -> c.CompanyName = "The Big Cheese"))
        |false -> (fun iq -> iq.Where(fun (c:CustomersEntity) -> c))
    queryable

let query2 =
    companyNameFilter true query1 |> Seq.toArray

    let qry1 =
        query { for u in dbContext.Users do
                select (u.Id, u.Name, u.Email)
        }

    let qry2 =
        query { for c in dbContext.Cars do
                select (c.UserId, c.Brand, c.Year)
        }

    query { for (i,n,e) in qry1 do
            join (u,b,y) in qry2 on (i = u)
            where (y > 2015)
            select (i,n,e,u,b,y)
        } |> Seq.toArray
