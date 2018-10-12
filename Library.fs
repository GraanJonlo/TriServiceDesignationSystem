namespace TriServiceDesignationSystem

module Common =
    type PositiverInteger = private PositiverInteger of int

    module PositiverInteger =
        let create x =
            if x > 0 then
                Some <| PositiverInteger x
            else
                None
        
        let vale (PositiverInteger x) = x
open Common

module Domain =
    type StatusPrefix =
        | PermanentlyGrounded
        | SpecialTestTemporary
        | SpecialTestPermanent
        | Experimental
        | Prototype
        | Planning
    
    let statusPrefixToLetter x =
        match x with
        | PermanentlyGrounded -> 'G'
        | SpecialTestTemporary -> 'J'
        | SpecialTestPermanent -> 'N'
        | Experimental -> 'X'
        | Prototype -> 'Y'
        | Planning -> 'Z'

    type ModifiedMission =
        | Attack
        | Cargo
        | Drone
        | ElectronicWarfare
        | Fighter
        | HelpAircraft
        | KeroseneCarrier
        | EquippedForColdWeatherOperations
        | MultiMission
        | Observation
        | MaritimePatrol
        | UnmannedAerialVehicle
        | Reconnaissance
        | SurfaceWarfare
        | Trainer
        | Utility
        | VipTransport
        | WeatherReconnaissance
    
    let modifiedMissionToLetter x =
        match x with
        | Attack -> 'A'
        | Cargo -> 'C'
        | Drone -> 'D'
        | ElectronicWarfare -> 'E'
        | Fighter -> 'F'
        | HelpAircraft -> 'H'
        | KeroseneCarrier -> 'K'
        | EquippedForColdWeatherOperations -> 'L'
        | MultiMission -> 'M'
        | Observation -> 'O'
        | MaritimePatrol -> 'P'
        | UnmannedAerialVehicle -> 'Q'
        | Reconnaissance -> 'R'
        | SurfaceWarfare -> 'S'
        | Trainer -> 'T'
        | Utility -> 'U'
        | VipTransport -> 'V'
        | WeatherReconnaissance -> 'W'
    
    type BasicMission =
        | AttackAircraft
        | Bomber
        | Transport
        | SpecialElectronicInstallation
        | Fighter
        | Tanker
        | LaserEquipped
        | Observation
        | MaritimePatrol
        | Reconnaissance
        | AntiSubmarineWarfare
        | Trainer
        | Utility
        | SpecialResearch
    
    let basicMissionToLetter x =
        match x with
        | AttackAircraft -> 'A'
        | Bomber -> 'B'
        | Transport -> 'C'
        | SpecialElectronicInstallation -> 'E'
        | Fighter -> 'F'
        | Tanker -> 'K'
        | LaserEquipped -> 'L'
        | Observation -> 'O'
        | MaritimePatrol -> 'P'
        | Reconnaissance -> 'R'
        | AntiSubmarineWarfare -> 'S'
        | Trainer -> 'T'
        | Utility -> 'U'
        | SpecialResearch -> 'X'
    
    type VehicleType =
        | UnmannedAerialVehicleControlSegment
        | Glider
        | Helicopter
        | UnmannedAerialVehicle
        | Spaceplane
        | VerticalTakeOffShortTakeOffAndLanding
        | LighterThanAir

    let vehicleTypeToLetter x =
        match x with
        | UnmannedAerialVehicleControlSegment -> 'D'
        | Glider -> 'G'
        | Helicopter -> 'H'
        | UnmannedAerialVehicle -> 'Q'
        | Spaceplane -> 'S'
        | VerticalTakeOffShortTakeOffAndLanding -> 'V'
        | LighterThanAir -> 'Z'

    type MissionDesignSeries = {
        statusPrefix: StatusPrefix option
        modifiedMission: ModifiedMission option
        basicMission: BasicMission
        vehicleType: VehicleType option
        designNumber: PositiverInteger
        seriesLetter: char
    }

    type TypeModelSeries = MissionDesignSeries
