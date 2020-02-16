module OrderPlacing.Tests

open OrderPlacing.OrderPlacing
open OrderPlacing.OrderPlacing.Domain
open Swensen.Unquote

let ``if product doesn't exist, validation fails`` () =
    let checkProductCodeExists (_address : string) : bool = false
    let checkAddressExists (_unvalidatedAddress : UnvalidatedAddress) : bool = true
    let unvalidatedOrder =
        {
            orderId = "12345"
            customerInfo = {
                firstName = "Felipe"
                lastName = "Nipo"
                email = "fpnipo@gmail.com"
            }
            shippingAddress = {
                addressLine1 = "1 O'Connell Street"
                addressLine2 = "Dublin"
                city = "Dublin"
                zipCode = "D01"
            }
            billingAddress = {
                addressLine1 = "1 O'Connell Street"
                addressLine2 = "Dublin"
                city = "Dublin"
                zipCode = "D01"
            }
            orderLines = List.empty
        } : UnvalidatedOrder
    
    raisesWith<exn> <@ Workflow.ValidateOrder.validateOrder checkProductCodeExists checkAddressExists unvalidatedOrder @>
                    (fun e -> <@ e.ToString() = "ProductCode must be valid" @>)