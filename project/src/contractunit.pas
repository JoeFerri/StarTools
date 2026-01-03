{
  This file is part of StarTools.
  StarTools is a fan-made suite of tools for Star Citizen.

  Copyright (c) 2025 Giuseppe Ferri <jfinfoit@gmail.com>

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU Affero General Public License as
  published by the Free Software Foundation, either version 3 of the
  License, or (at your option) any later version.

  See the file LICENSE, included in this distribution,
  for details about the copyright.

 **********************************************************************}


{
  NOTE:

    global.ini

    Covalex_HaulCargo_MultiToSingle_desc_ScrapWaste_Stanton3=Hi there,\n\nThere’s a few sites on ArcCorp that need their waste and scrap collected (all boxes <EM4>~mission(MissionMaxSCUSize)</EM4> or smaller) and taken to a freight elevator at <EM4>~mission(Destination|Address)</EM4>. Doesn’t matter what order you do the run, as long as you grab it all.    \n\nPICK UP LOCATIONS (ANY ORDER)\n\n~mission(MultiToSingleToken)\n\nYou know, my mom was a hauler who specialized in runs like this. It might not be the most glamorous stuff, but she claimed these runs had the best pay-to-risk ratio. Pirates never gave her any trouble when they found out what was on board.  \n\nBy the way, we strongly encourage contractors to bring a <EM4>handheld tractor beam</EM4> along.   \n\n~mission(Contractor|SignOff)\n\nChase Hewitt\nJr. Logistics Coordinator\nCovalex Shipping  \n'Anything you need, anywhere you need it.'\n\n\n\n\n\n\n\nCovalex Shipping is a limited liability corporation. To encourage timely deliveries and prevent fraud on cargo hauls, Covalex Shipping will set a delivery time limit and upon expiration, any undelivered cargo will be considered stolen and treated as such. Total payment will be prorated according to the percentage of cargo successfully delivered.

    Covalex_HaulCargo_AtoB_desc_RefinedOre_Stanton4=Hi, \n\n<EM4>~mission(Destination|Address)</EM4> needs a delivery of refined ore to keep their production lines humming. The cargo (<EM4>~mission(MissionMaxSCUSize) containers</EM4> or smaller) is ready and waiting for pick up from a freight elevator at <EM4>~mission(Location|address)</EM4>. Any chance you’re available to collect and deliver it to a freight elevator at ~mission(Destination)?\n\nBy the way, we strongly encourage contractors to bring a <EM4>handheld tractor beam</EM4> along.   \n\n~mission(Contractor|SignOff)\n\nChase Hewitt\nJr. Logistics Coordinator\nCovalex Shipping  \n'Anything you need, anywhere you need it.'\n\n\n\n\n\n\n\nCovalex Shipping is a limited liability corporation. To encourage timely deliveries and prevent fraud on cargo hauls, Covalex Shipping will set a delivery time limit and upon expiration, any undelivered cargo will be considered stolen and treated as such. Total payment will be prorated according to the percentage of cargo successfully delivered.

    --------------------------------------------------------------------------------

    Aggiunte le righe:
      star_citizen_version=4.4
      data_version=2024-16-12

    Remove non-compliant characters from global.ini to obtain global-mission.ini.

    --------------------------------------------------------------------------------
    Obtaining materials by parsing:
      mission_Item_nnnn=s  --> s
    example:
      mission_Item_0116=Neon   --> Neon

    Gli item sono troppi: implementare una UI che dia la possibilità all'utente
    di avere una finestra di item favoriti (più usati) e una finestra con tutti gli item

    ---------------------------------------------------------------------------------
    Obtain clients by parsing:
      mission_client_nnnn=s  --> s
    example:
      mission_client_0001=Shubin Interstellar  --> Shubin Interstellar
    NOT USED FOR NOW.

    ---------------------------------------------------------------------------------
    Obtain contractors by parsing:
      mission_contractor_nnnn=s  --> s
    example:
      mission_contractor_0012=Red Wind Linehaul  --> Red Wind Linehaul

    TODO: There are too many items: implement a UI that allows the user to have a window with favorite (most used) items and a window with all items.

    ---------------------------------------------------------------------------------

    mission_location_0124_add=Comm Array ST1-48 around Magda
    mission_location_0125_add=Comm Array ST1-92 around Aberdeen
    mission_location_0126_add=Comm Array ST1-13 around Arial
    mission_location_0127_add=Comm Array ST1-61 around Hurston

    mission_location_nyx_008=Glaciem Ring Wreck Site
    mission_location_nyx_008a=a wreck site in the Glaciem Ring
    mission_location_pyro_161a=Prospect Depot on Pyro III
    mission_location_stanton_0009=the mining facility on Yela
    mission_location_stanton1054a=Security Post Walvis on Yela
    mission_location_stanton_373=Rayari McGrath Research Outpost
    mission_location_stanton_secdep1=HDOF-Palomar
    mission_location_stanton_secdep1_add=HDOF-Palomar on Hurston
}

{* Provides tools for managing mission data, contracts, and routes. }
unit ContractUnit;

{$mode ObjFPC}{$H+}{$J-}{$R+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, Generics.Collections,
  SCUxSizeUnit, ConsoleUnit;




type
  {* Type of contract route. }
  TContractRouteType = (
    {* Undefined route. }
    crtUndefined,
    {* A to B route. }
    crtAtoB,
    {* Single to Multi route. }
    crtSingleToMulti,
    {* Multi to Single route. }
    crtMultiToSingle);

  {* Array of contract route types. }
  TContractRouteTypeArray = array[TContractRouteType] of String;

const
  {* Labels for TContractRouteType. }
  ContractRouteTypeArray: TContractRouteTypeArray = ('Undefined', 'A to B', 'Single to Multi', 'Multi to Single');

type
  {
    HaulCargo_CargoRoute_Interstellar=Interstellar
    HaulCargo_CargoRoute_Local=Local
    HaulCargo_CargoRoute_Planetary=Planetary
    HaulCargo_CargoRoute_Solar=Stellar
  }
  {* Type of contract cargo route. }
  TContractCargoRoute = (
    {* Undefined route. }
    ccrUNDEFINED,
    {* Local route. }
    ccrLOCAL,
    {* Planetary route. }
    ccrPLANETARY,
    {* Stellar route. }
    ccrSTELLAR,
    {* Interstellar route. }
    ccrINTERSTELLAR);

  {* Array of contract cargo route types. }
  TContractCargoRouteArray = array[TContractCargoRoute] of String;

const
  {* Labels for TContractCargoRoute. }
  ContractCargoRouteArray: TContractCargoRouteArray = ('Undefined', 'Local', 'Planetary', 'Stellar', 'Interstellar');

type
  {
    RepStanding_TransportGuild_Rank0=Trainee
    RepStanding_TransportGuild_Rank1=Rookie
    RepStanding_TransportGuild_Rank2=Junior
    RepStanding_TransportGuild_Rank3=Member
    RepStanding_TransportGuild_Rank4=Experienced
    RepStanding_TransportGuild_Rank5=Senior
    RepStanding_TransportGuild_Rank6=Master
  }
  {* Type of user rank. }
  TUserRank = (
    {* Undefined rank. }
    urUNDEFINED,
    {* Trainee rank. }
    urTRAINEE,
    {* Rookie rank. }
    urROOKIE,
    {* Junior rank. }
    urJUNIOR,
    {* Member rank. }
    urMEMBER,
    {* Experienced rank. }
    urEXPERIENCED,
    {* Senior rank. }
    urSENIOR,
    {* Master rank. }
    urMASTER);

  {* Array of user ranks. }
  TUserRankArray = array[TUserRank] of String;

const
  {* Labels for TUserRank. }
  UserRankArray: TUserRankArray = ('Undefined', 'Trainee', 'Rookie', 'Junior', 'Member', 'Experienced', 'Senior', 'Master');

type
  {
    HaulCargo_CargoGrade_Bulk=Large
    HaulCargo_CargoGrade_ExtraSmall=Extra Small
    HaulCargo_CargoGrade_Small=Small
    HaulCargo_CargoGrade_Supply=Medium
    HaulCargo_CargoScale_Bulk=Bulk
    HaulCargo_CargoScale_ExtraSmall=Extra Small
    HaulCargo_CargoScale_Large=Large
    HaulCargo_CargoScale_Medium=Medium
    HaulCargo_CargoScale_Small=Small
  }
  {* Type of contract cargo grade scale. }
  TContractCargoGradeScale = (
    {* Undefined scale. }
    ccgsUNDEFINED,
    {* Small scale. }
    ccgsSMALL,
    {* Extra Small scale. }
    ccgsEXTRA_SMALL,
    {* Medium scale. }
    ccgsMEDIUM,
    {* Large scale. }
    ccgsLARGE,
    {* Extra Large scale. }
    ccgsEXTRA_LARGE,
    {* Cargo Transfer from Shuttered Facility scale. }
    ccgsCARGO_TRANSFER_FROM_SHUTTERED_FACILITY,
    {* Small Scale. }
    ccgsSMALL_SCALE,
    {* Medium Shipment. }
    ccgsMEDIUM_SHIPMENT,
    {* Large Shipment. }
    ccgsLARGE_SHIPMENT,
    {* Bulk Shipment. }
    ccgsBULK_SHIPMENT);

  {* Array of contract cargo grade scale types. }
  TContractCargoGradeScaleArray = array[TContractCargoGradeScale] of String;

const
  {* Labels for TContractCargoGradeScale. }
  ContractCargoGradeScaleArray: TContractCargoGradeScaleArray =  ('Undefined',
                                                                  'Small', 'Extra Small', 'Medium', 'Large', 'Extra Large',
                                                                  'Cargo Transfer from Shuttered Facility',
                                                                  'Small Scale', 'Medium Shipment', 'Large Shipment', 'Bulk Shipment');

type
  {* Type of contract cargo grade scale direct. }
  TContractCargoGradeScaleDirect = (
    {* Undefined direct. }
    ccgsdUNDEFINED,
    {* Direct scale.}
    ccgsdDIRECT,
    {* No Direct scale. }
    ccgsdNO_DIRECT);

  {* Array of contract cargo grade scale direct types. }
  TContractCargoGradeScaleDirectArray = array[TContractCargoGradeScaleDirect] of String;

const
  {* Labels for TContractCargoGradeScaleDirect. }
  ContractCargoGradeScaleDirectArray: TContractCargoGradeScaleDirectArray = ('Undefined', 'Direct', 'No Direct');

type
  {* Type of contract transport. }
  TContractTransportType = (
    {* Undefined transport. }
    cttUNDEFINED,
    {* Hauling transport. }
    cttHAULING,
    {* Delivery transport. }
    cttDELIVERY);

  {* Array of contract transport types. }
  TContractTransportTypeArray = array[TContractTransportType] of String;

const
  {* Labels for TContractTransportType. }
  ContractTransportTypeArray: TContractTransportTypeArray = ('Undefined', 'Hauling', 'Delivery');

type
  {* Type of contract difficulty modifier. }
  TContractDifficultyModifier = (
    {* Undefined difficulty. }
    cdmUndefined,
    {* Regular difficulty. }
    cdmRegular,
    {* Combat difficulty. }
    cdmCombat,
    {* Illegal difficulty. }
    cdmIllegal);

  {* Array of contract difficulty modifier types. }
  TContractDifficultyModifierArray = array[TContractDifficultyModifier] of String;

const
  {* Labels for TContractDifficultyModifier. }
  ContractDifficultyModifierArray: TContractDifficultyModifierArray = ('Undefined', 'Regular', 'Combat', 'Illegal');



type
  {* Contains data relating to the loading station and the total number of crates loaded. }
  TLoadingRecord = record
    {* Crate loading station name. }
    StationName: String;
    {* Name of the goods. }
    Commodity: String;
    {* Total number of crates loaded. }
    SCU: Integer; //* >= 0
  end;


  {* Contains data for one of the crate drop-off stations,
      including the count of unloaded crates categorized by size. }
  TUnloadingRecord = record
    {* Crate unloading station name. }
    StationName: String;
    {* Name of the goods. }
    Commodity: String;
    {* SCU by size. }
    SCUxSize: TSCUxSizeRecord;
  end;


  {* Leg of Trade Route
      - Corresponds to a panel in the UI. 
      - Used within @link(TContractLeg) and @link(TContract).
      - SCU and @code(SCUMaxSize.TotalSCU) must be equal for consistency (see @link(TSCUxSizeRecord)).
  }
  TTradeRouteLeg = record
    {* Generic record grouping identifier.
        Panels with the same GroupId belong to the same contract leg. }
    GroupId: Integer; //* >= 0
    {* Crate loading station name. }
    LoadingStationName: String;
    {* Crate unloading station name. }
    UnloadingStationName: String;
    {* Name of the goods. }
    Commodity: String;
    {* Total number of SCUs to be delivered. }
    SCU: Integer; //* >= 0
    {* Maximum container size. }
    SCUMaxSize: TSCUSize;
    {* Route completion flag. }
    Done: Boolean;
    {* Route visibility flag. }
    Hide: Boolean;
    {* Record to keep track of the number of boxes for each SCU size. }
    SCUxSize: TSCUxSizeRecord;
  end;


  {* List of @link(TTradeRouteLeg). }
  TTradeRouteLegList = specialize TList<TTradeRouteLeg>;

  {* Array of @link(TTradeRouteLeg). }
  TTradeRouteLegArray = array of TTradeRouteLeg;


  {* Enumeration of utilities for @link(TTradeRouteLeg)}
  TTradeRouteLegItem = (
    {* Undefined. }
    eUndefined,
    {* Ignored. }
    eIgnored,
    {* GroupId. }
    eGroupId,
    {* GroupIdEnabled. }
    eGroupIdEnabled,
    {* LoadingStationName. }
    eLoadingStationName,
    {* UnloadingStationName. }
    eUnloadingStationName,
    {* Commodity. }
    eCommodity,
    {* SCU. }
    eSCU,
    {* SCUMaxSize. }
    eSCUMaxSize,
    {* Done. }
    eDone,
    {* Hide. }
    eHide,
    {* SCUxSize. }
    eSCUxSize);

  {* Array of @link(TTradeRouteLegItem). }
  TTradeRouteLegItemArray = array[TTradeRouteLegItem] of String;

const
  {* Labels for TTradeRouteLegItem. }
  TradeRouteLegItemArray: TTradeRouteLegItemArray =  ('Undefined', 'Ignored', 'GroupId', 'GroupIdEnabled', 'LoadingStationName', 'UnloadingStationName',
                                                      'Commodity', 'SCU', 'SCUMaxSize',
                                                      'Done', 'Hide', 'SCUxSize');

type
  {* Enumeration of utilities for @link(TTradeRouteLeg)}
  TTradeRouteLegItemError = (
    {* No error. }
    erNone,
    {* Ignored. }
    erIgnored,
    {* Generic error. }
    erGeneric,
    {* GroupId error. }
    erGroupId,
    {* LoadingStationName error. }
    erLoadingStationName,
    {* UnloadingStationName error. }
    erUnloadingStationName,
    {* Commodity error. }
    erCommodity,
    {* SCU error. }
    erSCU,
    {* SCUMaxSize error. }
    erSCUMaxSize,
    {* Done error. }
    erDone,
    {* Hide error. }
    erHide,
    {* SCUxSize error. }
    erSCUxSize);

const
  {* Labels for TTradeRouteLegItemError. }
  TradeRouteLegItemErrorArray: array[TTradeRouteLegItemError] of String =  ('None', 'Ignored', 'Generic Error', 'GroupId Error',
                                                                            'LoadingStationName Error', 'UnloadingStationName Error',
                                                                            'Commodity Error', 'SCU Error', 'SCUMaxSize Error',
                                                                            'Done Error', 'Hide Error', 'SCUxSize Error');

type
  {* Notification when a single trade route leg changes. Sender = @link(TPanelRow). }
  TTradeRouteLegChanged = procedure(Sender: TObject; AItem: TTradeRouteLegItem) of object;


  {* Array of @link(TUnloadingRecord). }
  TUnloadingRecordArray = array of TUnloadingRecord;



  {* String records for filling in @code(TCheckBox) }
  TItemsStringListRecord = record
    {* List of station names. }
    StationNames: TStringList;
    {* List of commodities. }
    Commodities: TStringList;
  end;

  {* Array of Trade Route Leg IDs. }
  TIDArray = array of Integer;

  {* Set of @code(Integer). }
  TIDSet = specialize THashSet<Integer>;


  {* Dictionary of @link(TTradeRouteLeg). }
  TTradeRouteLegDictionary = specialize TDictionary<Integer, TTradeRouteLeg>;
  {* Dictionary pair of @link(TTradeRouteLeg). }
  TTradeRouteLegDictionaryPair = TTradeRouteLegDictionary.TDictionaryPair;
  {* Array of dictionary pairs of @link(TTradeRouteLeg). }
  TTradeRouteLegDictionaryPairArray = array of TTradeRouteLegDictionaryPair;



  {* Represents a transport contract within a main contract. }
  TContractLeg = class
    private      
      FLoading: TLoadingRecord; // SCU calculated summing up all Unloadings
      FTradeRouteLegs: TTradeRouteLegDictionary; // same Loading, more than one Unloading
      FSCUMaxSize: TSCUSize;

      {* @returns(The Unloadings field as a @link(TUnloadingRecordArray).) }
      function GetUnloadings : TUnloadingRecordArray;

      {* @returns(Number of Unloadings in the Unloadings field.) }
      function GetUnloadingsCount: Integer;

    public
      {*
        Adds or sets a trade route leg.
        @param(ID The ID (@code(PanelID)) of the trade route leg.)
        @param(ATradeRouteLeg The trade route leg.)
        @param(AItemError The error of the trade route leg.)
      }
      procedure AddOrSetTradeRouteLeg(const ID: Integer; const ATradeRouteLeg: TTradeRouteLeg; out AItemError: TTradeRouteLegItemError);
      
      {*
        Removes a trade route leg.
        @param(ID The ID (@code(PanelID)) of the trade route leg.)
      }
      procedure RemoveTradeRouteLeg(const ID: Integer);
      
      {*
        Tries to get a trade route leg.
        @param(ID The ID (@code(PanelID)) of the trade route leg.)
        @param(ATradeRouteLeg The trade route leg.)
      }
      procedure TryGetTradeRouteLeg(const ID: Integer; out ATradeRouteLeg: TTradeRouteLeg);

      {* @returns(the IDs of all trade route legs.) }
      function GetTradeRouteLegIDs : TIDArray;
      
      {*
        @param(ID The ID (@code(PanelID)) of the trade route leg.)
        @returns(@code(True) if the trade route leg with the given ID exists.) }
      function TradeRouteLegsContainsID(const ID: Integer) : Boolean;
      
      {* @returns(the trade route legs as an array of dictionary pairs.) }
      function TradeRouteLegsToArray : TTradeRouteLegDictionaryPairArray;

      {*
        Creates a new TContractLeg instance.
        @param(ALoadingName The name of the loading station.)
        @param(ACommodity The commodity.)
        @param(ASCUMaxSize The maximum container size.)
        @raises(EArgumentException if ALoadingName or ACommodity is empty.)
      }
      constructor Create(const ALoadingName: String; const ACommodity: String; const ASCUMaxSize: TSCUSize); overload;
      
      {*
        Creates a new TContractLeg instance.
        @param(ALoading The loading station.)
        @param(ASCUMaxSize The maximum container size.)
      }
      constructor Create(const ALoading: TLoadingRecord; const ASCUMaxSize: TSCUSize); overload;
      
      {* Destructor. }
      destructor Destroy; override;




      {* Data relating to the loading station. }
      property Loading: TLoadingRecord read FLoading;

      {* Data relating to the unloading stations. }
      property Unloadings: TUnloadingRecordArray read GetUnloadings;

      {* Number of unloading stations. }
      property UnloadingsCount: Integer read GetUnloadingsCount;

      {* Maximum container size. }
      property SCUMaxSize: TSCUSize read FSCUMaxSize;

      {*
        Groups the unloading stations by station and commodity.
        TODO: generalize for the custom filter
        @param(StationCommodityGrouping If @code(True), the unloading stations are grouped by station and commodity.)
        @returns(@link(TUnloadingRecordArray) with all unloading stations.)}
      function GetGroupedUnloadings(const StationCommodityGrouping: Boolean): TUnloadingRecordArray;

      {* Returns a deep copy of the TContractLeg instance. }
      function Clone: TContractLeg;            

      {* Returns a string representation of the Loading field. }
      function LoadingToString : String;

      {* Returns a string representation of the Unloadings field. }
      function UnloadingsToString : String;

      {* Returns a string representation of the TContractLeg instance. }
      function ToString : String; override;

      {* Checks if the given TTradeRouteLeg conforms to the rules for being part of a TContractLeg. }
      class function IsConformedTradeRouteLeg(ATradeRouteLeg: TTradeRouteLeg) : Boolean; static;
  end;


  {* List of @link(TContractLeg). }
  TContractLegList = specialize TList<TContractLeg>;

  {* Array of @link(TContractLeg). }
  TContractLegArray = array of TContractLeg;








  {* Represents a complete contract, consisting of one or more station-to-station contracts. }
  TContract = class
    private
      FID: Integer; //* >= 0
      FContractor: String;
      FaUEC: Integer;
      FSCUMaxSize: TSCUSize;

      FContractLegs: TContractLegList;

      FContractRouteType: TContractRouteType; // dynamically updated
      FContractCargoRoute: TContractCargoRoute;
      FUserRank: TUserRank;
      FContractCargoGradeScale: TContractCargoGradeScale;
      FContractCargoGradeScaleDirect: TContractCargoGradeScaleDirect;
      FContractTransportType: TContractTransportType;
      FContractDifficultyModifier: TContractDifficultyModifier;

      {* Returns the number of contract legs. }
      function GetContractLegsCount : Integer;

      {* Returns the contract legs. }
      function GetContractLegs: TContractLegArray;

    public
      {* ID of the contract. }
      property ID: Integer read FID;

      {* Contractor }
      property Contractor: String read FContractor;

      {* Total mission payout in aUEC. }
      property aUEC: Integer read FaUEC; //* >= 0

      {* Maximum SCU size accepted for loading. }
      property SCUMaxSize: TSCUSize read FSCUMaxSize;

      {* Number of contract legs. }
      property ContractLegsCount : Integer read GetContractLegsCount;

      {* Contracts that make up the mission. }
      property ContractLegs: TContractLegArray read GetContractLegs;

      {* Type of contract route. }
      property ContractRouteType: TContractRouteType read FContractRouteType;

      {* Cargo route. }
      property ContractCargoRoute: TContractCargoRoute read FContractCargoRoute;
      
      {* User rank. }
      property UserRank: TUserRank read FUserRank;
      
      {* Cargo grade scale. }
      property ContractCargoGradeScale: TContractCargoGradeScale read FContractCargoGradeScale;
      
      {* Cargo grade scale direct. }
      property ContractCargoGradeScaleDirect: TContractCargoGradeScaleDirect read FContractCargoGradeScaleDirect;
      
      {* Transport type. }
      property ContractTransportType: TContractTransportType read FContractTransportType;
      
      {* Difficulty modifier. }
      property ContractDifficultyModifier: TContractDifficultyModifier read FContractDifficultyModifier;

      {*
        @param(LoadingStationName The name of the loading station.)
        @param(Commodity The name of the commodity.)
        @returns(@link(TContractLeg) with the given loading station name and commodity.)
      }
      function GetContractLeg(const LoadingStationName: String; const Commodity: String): TContractLeg;

      {*
        Adds a trade route leg.
        @param(const AContractLeg: TContractLeg)
        @raises(EContractException if the trade route leg is not conformed.)
      }
      procedure AddContractLeg(const AContractLeg: TContractLeg);

      {*
        Removes a trade route leg.
        @code(Leg) is set to the removed trade route leg or @code(nil) if the trade route leg was not found.
        @param(PanelID The ID (@code(PanelID)) of the trade route leg.)
        @param(Leg The trade route leg.)
        @returns(@code(True) if the trade route leg was removed.)
      }
      function RemoveTradeRouteLeg(const PanelID: Integer; out Leg: TContractLeg): Boolean;

      {*
        Removes a trade route Leg
        @param(AContractLeg The trade route leg.)
        @returns(@code(True) if the trade route leg was removed.)
      }
      function RemoveContractLeg(const AContractLeg: TContractLeg): Boolean;


      {* Returns a deep copy of the TContract instance. }
      function Clone: TContract;

      {* Returns a string representation of the TContract instance. }
      function ToString : String; override;

      {*
        Creates a new TContract instance.
        @param(const AID: Integer)
        @param(const ASCUMaxSize: TSCUSize)
      }
      constructor Create(const AID: Integer; const ASCUMaxSize: TSCUSize);

      {* Destructor. }
      destructor Destroy; override;

      {*
        Sets the console server.
        @param(AConsoleServer The console server.)
      }
      class procedure SetConsoleServer(AConsoleServer: TConsoleReaderThread); static;
  end;


  {* List of @link(TContract). }
  TContractArray = array of TContract;

  {* Dictionary of @link(TContract). }
  TContractDictionary = specialize TDictionary<Integer, TContract>;

  {* Dictionary pair of @link(TContract). }
  TContractDictionaryPair = TContractDictionary.TDictionaryPair;

  {* Array of dictionary pairs of @link(TContract). }
  TContractDictionaryPairArray = array of TContractDictionaryPair;

  {*
    Object dictionary of @link(TContract).
    @longCode(
      [doOwnsValues]             -> release the values
      [doOwnsKeys]               -> release the keys
      [doOwnsKeys, doOwnsValues] -> free both

      TContractObjectDictionary.Create([doOwnsValues]);
    )
  }
  TContractObjectDictionary = specialize TObjectDictionary<Integer, TContract>;

  {* Object dictionary pair of @link(TContract). }
  TContractObjectDictionaryPair = TContractObjectDictionary.TDictionaryPair;

  {* Array of object dictionary pairs of @link(TContract). }
  TContractObjectDictionaryPairArray = array of TContractObjectDictionaryPair;



  {* Exception class for contract-related errors. }
  EContractException = class(Exception)
    private
      FItemError: TTradeRouteLegItemError;
      FTradeRouteLeg: TTradeRouteLeg;
      FForAllLegs: Boolean;
    public
      {*
        Constructor.
        @param(AMsg The error message.)
        @param(AItemError The error of the trade route leg.)
        @param(ATradeRouteLeg The trade route leg.)
        @param(AForAllLegs If @code(True), the error applies to all legs.)
      }
      constructor Create(const AMsg: string; const AItemError: TTradeRouteLegItemError;
                          const ATradeRouteLeg: TTradeRouteLeg; const AForAllLegs: Boolean); reintroduce;
      {* Error type. }
      property ItemError: TTradeRouteLegItemError read FItemError;

      {* Trade route leg related to the error. Only the field associated with the error is relevant. }
      property TradeRouteLeg: TTradeRouteLeg read FTradeRouteLeg;
      
      {* Indicates if the error applies to all legs. }
      property ForAllLegs: Boolean read FForAllLegs;
    end;


  {*
    Notification when a single panel/leg changes. Sender = TContractDB.
    @param(Sender The sender object.)
    @param(PanelID The panel ID.)
    @param(AItem The trade route leg item.)
    @param(Leg The trade route leg.)
  }
  TPanelLegNotify = procedure(const Sender: TObject;
                              const PanelID: Integer;
                              const AItem: TTradeRouteLegItem;
                              const Leg: TTradeRouteLeg) of object;

  {*
    Notification when a contract changes. Sender = TContractDB.
    @param(Sender The sender object.)
    @param(GroupID The group ID.)
    @param(AContract The contract.)
  }
  TPanelContractNotify = procedure( const Sender: TObject;
                                    const GroupID: Integer;
                                    const AContract: TContract) of object;






implementation
  
var
  {* Console Server }
  Console: TConsoleReaderThread;



{ EContractException }

constructor EContractException.Create(const AMsg: string; const AItemError: TTradeRouteLegItemError;
                                      const ATradeRouteLeg: TTradeRouteLeg; const AForAllLegs: Boolean);
begin
  inherited Create(AMsg);
  FItemError := AItemError;
  FTradeRouteLeg := ATradeRouteLeg;
  FForAllLegs := AForAllLegs;
end;



{ TContractLeg }

constructor TContractLeg.Create(const ALoadingName: String; const ACommodity: String; const ASCUMaxSize: TSCUSize);
begin
  if ALoadingName = '' then
    raise EArgumentException.Create('The ALoadingName parameter cannot be empty.');

  if ACommodity = '' then
    raise EArgumentException.Create('The ACommodity parameter cannot be empty.');

  inherited Create;

  FSCUMaxSize := ASCUMaxSize;

  FLoading.StationName := ALoadingName;
  FLoading.Commodity := ACommodity;
  FTradeRouteLegs := TTradeRouteLegDictionary.Create;
end;



constructor TContractLeg.Create(const ALoading: TLoadingRecord; const ASCUMaxSize: TSCUSize);
begin
  Create(ALoading.StationName, ALoading.Commodity, ASCUMaxSize);
end;



destructor TContractLeg.Destroy;
begin
  FTradeRouteLegs.Clear;
  FTradeRouteLegs.Free;
  inherited Destroy;
end;



function TContractLeg.GetUnloadings: TUnloadingRecordArray;
var
  Index: Integer;
  Value: TTradeRouteLegDictionaryPair.TValue;
begin
  Result := nil;
  SetLength(Result, FTradeRouteLegs.Count);
  Index := 0;
  for Value in FTradeRouteLegs.Values do
  begin
    Result[Index].StationName := Value.UnloadingStationName;
    Result[Index].Commodity   := Value.Commodity;
    Result[Index].SCUxSize    := Value.SCUxSize;
    Inc(Index);
  end;
end;



function TContractLeg.GetUnloadingsCount: Integer;
begin
  Result := FTradeRouteLegs.Count;
end;



function TContractLeg.GetGroupedUnloadings(const StationCommodityGrouping: Boolean): TUnloadingRecordArray;
var
  Value: TTradeRouteLegDictionaryPair.TValue;
  GroupedDict: specialize TDictionary<String, TUnloadingRecord>; // Utilizzare un puntatore ^TUnloadingRecord nel caso di grandi quantità di dati
  RecordData: TUnloadingRecord;
  i: Integer;
  Key: string;
begin
  Result := nil;
  if FTradeRouteLegs.Count = 0 then Exit;

  GroupedDict := specialize TDictionary<String, TUnloadingRecord>.Create;
  try
    for Value in FTradeRouteLegs.Values do
    begin
      if StationCommodityGrouping then
        Key := Value.UnloadingStationName + ' ' + Value.Commodity
      else
        Key := Value.UnloadingStationName;

      if GroupedDict.TryGetValue(Key, RecordData) then
      begin
        RecordData.SCUxSize := RecordData.SCUxSize + Value.SCUxSize;
        GroupedDict.Items[Key] := RecordData;
      end
      else
      begin
        RecordData := Default(TUnloadingRecord);
        RecordData.StationName := Value.UnloadingStationName;
        RecordData.Commodity   := Value.Commodity;
        RecordData.SCUxSize    := Value.SCUxSize;
        GroupedDict.Add(Key, RecordData);
      end;
    end;

    SetLength(Result, GroupedDict.Count);
    i := 0;
    for RecordData in GroupedDict.Values do
    begin
      Result[i] := RecordData;
      Inc(i);
    end;

  finally
    GroupedDict.Free;
  end;
end;



procedure TContractLeg.AddOrSetTradeRouteLeg(const ID: Integer; const ATradeRouteLeg: TTradeRouteLeg; out AItemError: TTradeRouteLegItemError);
var
  TradeRouteLeg: TTradeRouteLeg;
begin
  AItemError := erNone;

  if FLoading.StationName <> ATradeRouteLeg.LoadingStationName then
  begin
    Console.DebugLog('TContractLeg.AddOrSetTradeRouteLeg',
                      Format('FLoading.StationName: %s <> ATradeRouteLeg.LoadingStationName: %s',
                        [FLoading.StationName, ATradeRouteLeg.LoadingStationName]));
    AItemError := erLoadingStationName;
    Exit;
  end;

  if FLoading.Commodity <> ATradeRouteLeg.Commodity then
  begin
    Console.DebugLog('TContractLeg.AddOrSetTradeRouteLeg',
                      Format('FLoading.Commodity: %s <> ATradeRouteLeg.Commodity: %s',
                        [FLoading.Commodity, ATradeRouteLeg.Commodity]));
    AItemError := erCommodity;
    Exit;
  end;

  if FSCUMaxSize <> ATradeRouteLeg.SCUMaxSize then
  begin
    Console.DebugLog('TContractLeg.AddOrSetTradeRouteLeg',
                      Format('FSCUMaxSize: %d <> ATradeRouteLeg.SCUMaxSize: %d',
                        [SCUSizeArray[FSCUMaxSize], SCUSizeArray[ATradeRouteLeg.SCUMaxSize]]));
    AItemError := erSCUMaxSize;
    Exit;
  end;

  for TradeRouteLeg in FTradeRouteLegs.Values do
    if TradeRouteLeg.UnloadingStationName = ATradeRouteLeg.UnloadingStationName then
    begin
      Console.DebugLog('TContractLeg.AddOrSetTradeRouteLeg',
                        Format('TradeRouteLeg.UnloadingStationName: %s = ATradeRouteLeg.UnloadingStationName: %s',
                          [TradeRouteLeg.UnloadingStationName, ATradeRouteLeg.UnloadingStationName]));
      AItemError := erUnloadingStationName;
      Exit;
    end;

  FTradeRouteLegs.AddOrSetValue(ID, ATradeRouteLeg);
  FLoading.SCU := FLoading.SCU + ATradeRouteLeg.SCU;
end;



procedure TContractLeg.RemoveTradeRouteLeg(const ID: Integer);
begin
  if FTradeRouteLegs.ContainsKey(ID) then
  begin
    FLoading.SCU := FLoading.SCU - FTradeRouteLegs.Items[ID].SCU;
    FTradeRouteLegs.Remove(ID);
  end;
end;



procedure TContractLeg.TryGetTradeRouteLeg(const ID: Integer; out ATradeRouteLeg: TTradeRouteLeg);
begin
  ATradeRouteLeg := Default(TTradeRouteLeg);
  FTradeRouteLegs.TryGetValue(ID, ATradeRouteLeg);
end;



function TContractLeg.GetTradeRouteLegIDs : TIDArray;
begin
  Result := FTradeRouteLegs.Keys.ToArray;
end;



function TContractLeg.TradeRouteLegsContainsID(const ID: Integer) : Boolean;
begin
  Result := FTradeRouteLegs.ContainsKey(ID);
end;



function TContractLeg.TradeRouteLegsToArray : TTradeRouteLegDictionaryPairArray;
begin
  Result := FTradeRouteLegs.ToArray;
end;



class function TContractLeg.IsConformedTradeRouteLeg(ATradeRouteLeg: TTradeRouteLeg) : Boolean;
begin
  Result :=   (not String.IsNullOrWhiteSpace(ATradeRouteLeg.LoadingStationName))   and
              (not String.IsNullOrWhiteSpace(ATradeRouteLeg.UnloadingStationName)) and
              (not String.IsNullOrWhiteSpace(ATradeRouteLeg.Commodity))            and
              (ATradeRouteLeg.SCU > 0)                                             and
              (ATradeRouteLeg.SCUxSize.TotalSCU > 0);
end;



function TContractLeg.Clone: TContractLeg;
var
  Pair: TTradeRouteLegDictionaryPair;
begin
  Result := TContractLeg.Create(FLoading, FSCUMaxSize);
  Result.FLoading.SCU := Self.FLoading.SCU;

  for Pair in FTradeRouteLegs do
  begin
    Result.FTradeRouteLegs.Add(Pair.Key, Pair.Value);
  end;
end;



function TContractLeg.UnloadingsToString : String;
var
  UnloadingRecord: TUnloadingRecord;
  UnloadingsRecordArray: TUnloadingRecordArray;
begin
  Result := '';

  UnloadingsRecordArray := GetUnloadings;

  for UnloadingRecord in UnloadingsRecordArray do
  begin
    Result += Format('  Unloading: %s' + LineEnding,
                      [UnloadingRecord.StationName]);
    Result += Format('  TotalSCU: %d' + LineEnding + LineEnding,
                      [UnloadingRecord.SCUxSize.TotalSCU]);
    Result += Format('    SCUSize %d: %d' + LineEnding,
                      [32, UnloadingRecord.SCUxSize.SCUSize32]);
    Result += Format('    SCUSize %d: %d' + LineEnding,
                      [24, UnloadingRecord.SCUxSize.SCUSize24]);
    Result += Format('    SCUSize %d: %d' + LineEnding,
                      [16, UnloadingRecord.SCUxSize.SCUSize16]);
    Result += Format('    SCUSize %d: %d' + LineEnding,
                      [08, UnloadingRecord.SCUxSize.SCUSize08]);
    Result += Format('    SCUSize %d: %d' + LineEnding,
                      [04, UnloadingRecord.SCUxSize.SCUSize04]);
    Result += Format('    SCUSize %d: %d' + LineEnding,
                      [02, UnloadingRecord.SCUxSize.SCUSize02]);
    Result += Format('    SCUSize %d: %d' + LineEnding + LineEnding,
                      [01, UnloadingRecord.SCUxSize.SCUSize01]);
  end;
  Result += LineEnding;
end;



function TContractLeg.LoadingToString : String;
begin
  Result := '';

  Result += Format('Loading: %s' + LineEnding,
                    [FLoading.StationName]);
  Result += Format('Commodity: %s' + LineEnding,
                    [FLoading.Commodity]);
  Result += Format('SCU: %d' + LineEnding + LineEnding,
                    [FLoading.SCU]);
end;



function TContractLeg.ToString : String;
begin
  Result := '';

  Result += LoadingToString;

  Result += UnloadingsToString;
end;






{ TContract }

class procedure TContract.SetConsoleServer(AConsoleServer: TConsoleReaderThread);
begin
  Console := AConsoleServer;
end;



function TContract.GetContractLegsCount : Integer;
begin
  Result := FContractLegs.Count;
end;



function TContract.GetContractLegs: TContractLegArray;
begin
  Result := FContractLegs.ToArray;
end;



function TContract.GetContractLeg(const LoadingStationName: String; const Commodity: String): TContractLeg;
var
  ContractLeg: TContractLeg;
begin
  Result := nil;
  for ContractLeg in FContractLegs do
    if (ContractLeg.Loading.StationName = LoadingStationName) and (ContractLeg.Loading.Commodity = Commodity) then
    begin
      Result := ContractLeg;
      break;
    end;
end;



function TContract.RemoveTradeRouteLeg(const PanelID: Integer; out Leg: TContractLeg): Boolean;
var
  ContractLeg: TContractLeg;
begin
  Result := False;
  Leg := nil;
  for ContractLeg in FContractLegs do
    if ContractLeg.TradeRouteLegsContainsID(PanelID) then
    begin
      Result := True;
      Leg := ContractLeg;
      ContractLeg.RemoveTradeRouteLeg(PanelID);
      break;
    end;
end;



function TContract.RemoveContractLeg(const AContractLeg: TContractLeg): Boolean;
begin
  Result := False;
  Result := FContractLegs.Remove(AContractLeg) <> -1;
end;



procedure TContract.AddContractLeg(const AContractLeg: TContractLeg);
var
  Finded: Boolean;
  ContractLeg: TContractLeg;
  TradeRouteLeg: TTradeRouteLeg;
begin
  TradeRouteLeg := default(TTradeRouteLeg);

  if AContractLeg.SCUMaxSize <> FSCUMaxSize then
  begin
    Console.DebugLog('TContract.AddOrUpdateContractLeg',
                      Format('FSCUMaxSize: %d <> AContractLeg.SCUMaxSize: %d',
                        [SCUSizeArray[FSCUMaxSize], SCUSizeArray[AContractLeg.SCUMaxSize]]));
    raise EContractException.Create('The AContractLeg.SCUMaxSize does not match the TContract.SCUMaxSize.', erSCUMaxSize, TradeRouteLeg, True);
  end;

  Finded := False;
  for ContractLeg in FContractLegs do
    if (ContractLeg.Loading.StationName = AContractLeg.Loading.StationName) and (ContractLeg.Loading.Commodity = AContractLeg.Loading.Commodity) then
    begin
      Finded := True;
      Break;
    end;

  if not Finded then
    FContractLegs.Add(AContractLeg)
  else
  begin
    Console.DebugLog('TContract.AddOrUpdateContractLeg',
                      Format('ContractLeg with Loading: %s, Commodity: %s already exists',
                        [AContractLeg.Loading.StationName, AContractLeg.Loading.Commodity]));
  end;
end;



function TContract.ToString : String;
var
  ContractLeg: TContractLeg;
begin
  Result := '';
  ContractLeg := nil;

  Result += Format('Contract ID: %d', [FID]) + LineEnding + LineEnding;

  Result += Format('Contractor: %s' + LineEnding, [FContractor]);
  Result += Format('Rewards: %d aUEC' + LineEnding,
                    [FaUEC]);
  Result += Format('SCU Max Size: %d' + LineEnding + LineEnding,
                    [SCUSizeArray[FSCUMaxSize]]);

  Result += Format('Contract Route Type: %s' + LineEnding, [ContractRouteTypeArray[FContractRouteType]]);
  Result += Format('Contract Cargo Route: %s' + LineEnding, [ContractCargoRouteArray[FContractCargoRoute]]);
  Result += Format('User Rank: %s' + LineEnding, [UserRankArray[FUserRank]]);
  Result += Format('Contract Cargo Grade Scale: %s' + LineEnding, [ContractCargoGradeScaleArray[FContractCargoGradeScale]]);
  Result += Format('Contract Cargo Grade Scale Direct: %s' + LineEnding, [ContractCargoGradeScaleDirectArray[FContractCargoGradeScaleDirect]]);
  Result += Format('Contract Transport Type: %s' + LineEnding, [ContractTransportTypeArray[FContractTransportType]]);
  Result += Format('Contract Difficulty Modifier: %s' + LineEnding, [ContractDifficultyModifierArray[FContractDifficultyModifier]]);

  Result += LineEnding;

  for ContractLeg in FContractLegs do
  begin
    Result += ContractLeg.ToString;
  end;
end;



constructor TContract.Create(const AID: Integer; const ASCUMaxSize: TSCUSize);
begin
  inherited Create;

  FID := AID;
  FContractor := '';
  FaUEC := 0;
  FSCUMaxSize := ASCUMaxSize;
  FContractLegs := TContractLegList.Create;
end;



destructor TContract.Destroy;
var
  i: Integer;
begin

  for i := FContractLegs.Count - 1 downto 0 do
  begin
    FContractLegs[i].Free;
    FContractLegs.Delete(i);
  end;

  FContractLegs.Free;
  FContractLegs := nil;

  inherited Destroy;
end;



function TContract.Clone: TContract;
begin
  Result := TContract.Create(FID, SCUMaxSize);
  Result.FaUEC := Self.FaUEC;
  Result.FSCUMaxSize := Self.FSCUMaxSize;
  Result.FContractLegs.AddRange(FContractLegs);
end;



end.



