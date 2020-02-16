module OrderPlacing.OrderPlacing

open OrderPlacing.Types
open System    

module Domain =

    type Email = private Email of string
    module Email =
        let create str =
            if String.IsNullOrEmpty str then
                failwith "Email must not be null or empty"
            elif String.length str > 50 then
                failwith "Email must not be more than 50 chars"
            else
                Email str
        let value (Email str) = str
    
    type ZipCode = private ZipCode of string
    module ZipCode =
        let create str =
            if String.IsNullOrEmpty str then
                failwith "Zipcode must not be null or empty"
            elif String.length str > 50 then
                failwith "Zipcode must not be more than 7 chars"
            else
                ZipCode str
        let value (ZipCode str) = str
    
    type OrderId = private OrderId of string
    module OrderId =
        let create str =
            if String.IsNullOrEmpty str then
                failwith "OrderId must not be null or empty"
            elif String.length str > 50 then
                failwith "OrderId must not be more than 50 chars"
            else
                OrderId str
        let value (OrderId str) = str
    
    type OrderLineId = private OrderLineId of string
    module OrderLineId =
        let create str =
            if String.IsNullOrEmpty str then
                failwith "OrderLineId must not be null or empty"
            elif String.length str > 50 then
                failwith "OrderLineId must not be more than 50 chars"
            else
                OrderLineId str
        let value (OrderLineId str) = str
    
    type ProductCode =
        | Widget of string
        | Gizmo of string
    module ProductCode =
        let create str =
            if String.IsNullOrEmpty str then
                failwith "ProductCode must not be null or empty"
            match str.Chars 0 with
            | 'W' -> ProductCode.Widget str
            | 'G' -> ProductCode.Gizmo str
            | _ -> failwith "Invalid ProductCode"
    
    type UnitQuantity = private UnitQuantity of int
    module UnitQuantity =
        let create value =
            if value < 0 then
                failwith "UnitQuantity must not be negative"
            else
                UnitQuantity value
        let value (UnitQuantity value) = value
    
    type KilogramQuantity = private KilogramQuantity of int
    module KilogramQuantity =
        let create value =
            if value < 0 then
                failwith "KilogramQuantity must not be negative"
            else
                KilogramQuantity value
        let value (KilogramQuantity value) = value
    
    type OrderQuantity =
        | Unit of UnitQuantity
        | Kilogram of KilogramQuantity
    
    type UnvalidatedOrder = {
        orderId : string
        customerInfo : UnvalidatedCustomerInfo
        shippingAddress : UnvalidatedAddress
        billingAddress : UnvalidatedAddress
        orderLines : UnvalidatedOrderLine list
    } and UnvalidatedCustomerInfo = {
        firstName : string
        lastName : string
        email : string
    } and UnvalidatedAddress = {
        addressLine1 : string
        addressLine2 : string
        city : string
        zipCode : string
    } and UnvalidatedOrderLine = {
        orderLineId : string
        productCode : string
        quantity : string
    }
    
    type ValidatedOrder =  {
        orderId : OrderId
        customerInfo : ValidatedCustomerInfo
        shippingAddress : ValidatedAddress
        billingAddress : ValidatedAddress
        orderLines : ValidatedOrderLine list
    } and ValidatedCustomerInfo = {
        firstName : string
        lastName : string
        email : Email
    } and ValidatedAddress = {
        addressLine1 : string
        addressLine2 : string
        city : string
        zipCode : ZipCode
    } and ValidatedOrderLine = {
        orderLineId : OrderLineId
        productCode : ProductCode
        quantity : OrderQuantity
    }
    
    type PricedOrder = {
        orderId : OrderId
        customerInfo : ValidatedCustomerInfo
        shippingAddress : ValidatedAddress
        billingAddress : ValidatedAddress
        orderLines : PricedOrderLine list
        amountToBill : decimal
    } and PricedOrderLine = {
        orderLineId : OrderLineId
        productCode : ProductCode
        quantity : OrderQuantity
        totalPrice : decimal
    }
    
    type Order =
        | Unvalidated of UnvalidatedOrder
        | Validated of ValidatedOrder
        | Priced of PricedOrder
    
    type HtmlString = HtmlString of string
    
    type OrderAcknowledgment = {
        email : Email
        letter : HtmlString
    }
    
    type OrderAcknowledgmentSent = {
        orderId : OrderId
        email : Email
    }
    
    type SendResult = Sent | NotSent
 
module Events =
    
    open Domain
    
    type OrderPlaced = PricedOrder
    
    type BillableOrderPlaced = {
        orderId : OrderId
        billingAddress : ValidatedAddress
        amountToBill : decimal
    }
    
    type PlaceOrderEvent =
        | OrderPlaced of OrderPlaced
        | BillableOrderPlaced of BillableOrderPlaced
        | AcknowledgmentSent of OrderAcknowledgmentSent
    
    type CreateEvents = PricedOrder -> OrderAcknowledgmentSent option -> PlaceOrderEvent list
    
    let createBillingEvent (placedOrder : PricedOrder) : BillableOrderPlaced option =
        if (placedOrder.amountToBill < 0M) then None
        else
            Some <| {
                orderId = placedOrder.orderId
                billingAddress = placedOrder.billingAddress
                amountToBill = placedOrder.amountToBill
            }
    
    let createEvents : CreateEvents =
        fun pricedOrder acknowledgmentEventOpt ->
            let event1 = pricedOrder |> PlaceOrderEvent.OrderPlaced
            let event2Opt = acknowledgmentEventOpt |> Option.map PlaceOrderEvent.AcknowledgmentSent
            let event3Opt = pricedOrder |> createBillingEvent |> Option.map PlaceOrderEvent.BillableOrderPlaced
            
            [
                yield event1
                yield! event2Opt |> (function | Some x -> [x] | None -> [])
                yield! event3Opt |> (function | Some x -> [x] | None -> [])
            ]
    
module Workflow =
    
    open Domain
    
    // Using function types
    type CheckAddressExists = UnvalidatedAddress -> bool
    type CheckProductCodeExists = string -> bool
    type GetProductPrice = (ProductCode -> decimal)
    type ValidateOrder = (string -> bool) -> (UnvalidatedAddress -> bool) -> UnvalidatedOrder -> ValidatedOrder
    type PriceOrderLine = GetProductPrice -> ValidatedOrderLine -> PricedOrderLine
    type PriceOrder = GetProductPrice -> ValidatedOrder -> PricedOrder
    type CreateOrderAcknowledgmentLetter = PricedOrder -> HtmlString
    type SendOrderAcknowledgment = OrderAcknowledgment -> SendResult
    type AcknowledgeOrder = CreateOrderAcknowledgmentLetter -> SendOrderAcknowledgment -> PricedOrder -> OrderAcknowledgmentSent option
    type PlaceOrderWorkflow = UnvalidatedOrder -> Events.PlaceOrderEvent list
    
    module ValidateOrder =
    
        let toCustomerInfo (unvalidatedCustomerInfo : UnvalidatedCustomerInfo) : ValidatedCustomerInfo = 
            let firstName = unvalidatedCustomerInfo.firstName |> String50.tryCreate
            let lastName = unvalidatedCustomerInfo.lastName |> String50.tryCreate
            let email = unvalidatedCustomerInfo.email |> Email.create
            
            match firstName, lastName with
            | None, _ -> failwith "" // TODO
            | _, None -> failwith "" // TODO
            | Some firstName, Some lastName ->
            
            {
                firstName = firstName |> String50.value 
                lastName = lastName |> String50.value
                email = email
            }
        
        let toAddress (checkAddressExists : CheckAddressExists) unvalidatedAddress =
            if not (checkAddressExists unvalidatedAddress) then
                failwith "" // TODO
            
            let addressLine1 = unvalidatedAddress.addressLine1 |> String50.tryCreate
            let addressLine2 = unvalidatedAddress.addressLine2 |> String50.tryCreate
            let city = unvalidatedAddress.city |> String50.tryCreate
            let zipCode = unvalidatedAddress.zipCode |> ZipCode.create
            
            match addressLine1, addressLine2, city with
            | None, _, _ -> failwith "" // TODO
            | _, None, _ -> failwith "" // TODO
            | _, _, None -> failwith "" // TODO
            | Some addressLine1, Some addressLine2, Some city ->
            
            {
                addressLine1 = String50.value addressLine1
                addressLine2 = String50.value addressLine2
                city = String50.value city
                zipCode = zipCode
            }
        
        let toProductCode (checkProductCodeExists : CheckProductCodeExists) productCode =
            if checkProductCodeExists productCode then
                productCode |> ProductCode.create |> Some
            else
                None
        
        let toOrderQuantity productCode quantity =
            match productCode with
            | Widget _code -> quantity |> int |> UnitQuantity.create |> OrderQuantity.Unit
            | Gizmo _code -> quantity |> int |> KilogramQuantity.create |> OrderQuantity.Kilogram
        
        let toValidatedOrderLine checkProductCodeExists (unvalidatedOrderLine : UnvalidatedOrderLine) =
            let orderLineId = unvalidatedOrderLine.orderLineId |> OrderLineId.create
            let productCode = unvalidatedOrderLine.productCode |> toProductCode checkProductCodeExists
            match productCode with
            | None -> failwith "ProductCode must be valid"
            | Some productCode ->

            let quantity = unvalidatedOrderLine.quantity |> toOrderQuantity productCode
            {
                orderLineId = orderLineId
                productCode = productCode
                quantity = quantity
            }
            
        let validateOrder : ValidateOrder =
            fun checkProductCodeExists checkAddressExists unvalidatedOrder ->
                let orderId = unvalidatedOrder.orderId |> OrderId.create
                let customerInfo = toCustomerInfo unvalidatedOrder.customerInfo  
                let shippingAddress = unvalidatedOrder.shippingAddress |> toAddress checkAddressExists
                let billingAddress = unvalidatedOrder.shippingAddress |> toAddress checkAddressExists
                let orderLines = unvalidatedOrder.orderLines |> List.map (toValidatedOrderLine checkProductCodeExists)
                
                {
                    orderId = orderId
                    customerInfo = customerInfo
                    shippingAddress = shippingAddress
                    billingAddress = billingAddress
                    orderLines = orderLines
                }
    
    module PriceOrder =
    
        let toPricedOrderLine : PriceOrderLine =
            fun getProductPrice validatedOrderLine ->
                let quantityValue =
                    validatedOrderLine.quantity
                    |> (function OrderQuantity.Kilogram v -> KilogramQuantity.value v | OrderQuantity.Unit v -> UnitQuantity.value v)
                let price = validatedOrderLine.productCode |> getProductPrice
                let linePrice = price * (Decimal quantityValue)
                {
                    orderLineId = validatedOrderLine.orderLineId
                    productCode = validatedOrderLine.productCode
                    quantity = validatedOrderLine.quantity
                    totalPrice = linePrice
                }
            
        let priceOrder : PriceOrder =
            fun getProductPrice validatedOrder ->
                let lines = validatedOrder.orderLines |> List.map (toPricedOrderLine getProductPrice)
                let amountToBill = lines |> List.map (fun line -> line.totalPrice) |> List.sum
                {
                    orderId = validatedOrder.orderId
                    customerInfo = validatedOrder.customerInfo
                    shippingAddress = validatedOrder.shippingAddress
                    billingAddress = validatedOrder.billingAddress
                    orderLines = lines
                    amountToBill = amountToBill
                }
    
    module AcknowledgeOrder =
    
        let acknowledgeOrder : AcknowledgeOrder =
            fun createAcknowledgmentLetter sendAcknowledgment pricedOrder ->
                let letter = createAcknowledgmentLetter pricedOrder
                let acknowledgment = {
                    email = pricedOrder.customerInfo.email
                    letter = letter
                }
                
                match sendAcknowledgment acknowledgment with
                | Sent ->
                    Some <| {
                        orderId = pricedOrder.orderId
                        email = pricedOrder.customerInfo.email
                    }
                | NotSent -> None
            
    let placeOrder checkProductCodeExists checkAddressExists getProductPrice
                    createOrderAcknowledgmentLetter sendAcknowledgment : PlaceOrderWorkflow =
        let validateOrder' = ValidateOrder.validateOrder checkProductCodeExists checkAddressExists
        let priceOrder' = PriceOrder.priceOrder getProductPrice
        let acknowledgeOrder' = AcknowledgeOrder.acknowledgeOrder createOrderAcknowledgmentLetter sendAcknowledgment
        
        fun unvalidatedOrder ->
            unvalidatedOrder
            |> validateOrder'
            |> priceOrder'
            |> (fun pricedOrder ->
                    let acknowledgedOrder = acknowledgeOrder' pricedOrder
                    Events.createEvents pricedOrder acknowledgedOrder)