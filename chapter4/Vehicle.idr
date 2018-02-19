data PowerSource = Petrol | Pedal

data Rego = Registered | Unregistered

data Vehicle   : Rego -> PowerSource -> Type where
  BicycleReg   : Vehicle Registered   Pedal
  BicycleUnReg : Vehicle Unregistered Pedal
  CarReg       : (fuel: Nat) -> Vehicle Registered   Petrol
  CarUnReg     : (fuel: Nat) -> Vehicle Unregistered Petrol
  BusReg       : (fuel: Nat) -> Vehicle Registered   Petrol
  BusUnReg     : (fuel: Nat) -> Vehicle Unregistered Petrol

refuel : Vehicle r Petrol -> Vehicle r Petrol
refuel (CarReg   fuel)   = CarReg   (fuel + 100)
refuel (CarUnReg fuel) = CarUnReg (fuel + 50)
refuel (BusReg   fuel)   = BusReg   (fuel + 200)
refuel (BusUnReg fuel) = BusUnReg (fuel + 150)

-- refuel only Registered Petrol Vehicles
refuelR : Vehicle Registered Petrol -> Vehicle Registered Petrol
refuelR (CarReg fuel) = CarReg (fuel + 100)
refuelR (BusReg fuel) = BusReg (fuel + 200)
-- impossible doesn't seem to work across multiple dependent types
-- refuelR CarUnReg      impossible
-- refuelR BusUnReg      impossible
-- refuelR BicycleReg    impossible
-- refuelR BicycleUnReg  impossible
