{
  This file is part of StarTools.
  StarTools is a fan-made suite of tools for Star Citizen.

  Copyright (c) 2025-2026 Giuseppe Ferri <jfinfoit@gmail.com>

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU Affero General Public License as
  published by the Free Software Foundation, either version 3 of the
  License, or (at your option) any later version.

  See the file LICENSE, included in this distribution,
  for details about the copyright.

 **********************************************************************}



{* Lightweight in-memory DB for Contracts and TradeRouteLegs.

  - TContractDB manages:
      * Contracts (mapped by GroupId)
      * TradeRouteLegs (temporary "limbo" legs mapped by PanelID)
      * Panel subscriptions (panels register to receive notifications)

  Design notes:
  - Panels register with RegisterPanel and receive a unique PanelID.
  - Panels provide two optional callbacks: one for leg/row updates (OnLegNotify)
    and one for contract-level updates (OnContractNotify).
  - When a panel pushes an updated TTradeRouteLeg to the DB via PanelChanged,
    if Leg.GroupId = 0 the leg is kept in the TTradeRouteLeg mapping; if
    Leg.GroupId > 0 the DB will create or update a TContract for that GroupId,
    convert the leg into a TContractLeg and add/remove it from the contract.
  - If a panel moves from a GroupId > 0 back to 0, the associated contract leg is
    removed from the contract and the route leg is returned to the limbo mapping.

  - We assume that all changes to the status of the system stored in the database occur within limbo;
    to achieve this, we modify the GroupID of the calling panel by setting it to 0
    and only operate in the instruction block with AItem = @link(eGroupID).
*}
unit ContractDBUnit;

{$mode objfpc}{$H+}{$J-}{$R+}

interface

uses
  Classes, SysUtils, Generics.Collections,
  ContractUnit, SCUxSizeUnit, SizeDialogUnit,
  Controls, Dialogs,
  ConsoleUnit;




type
  {* Internal subscription object for panels. }
  TPanelSubscriber = class
  public
    {* Panel ID }
    PanelID: Integer;

    {* Group ID }
    
    GroupID: Integer; // current GroupId the panel is attached to; 0 = limbo
    {* Leg }
    
    Leg: TContractLeg;
    {* Callback for leg updates }
    
    OnLegNotify: TPanelLegNotify;
    {* Callback for contract updates }
    
    OnContractNotify: TPanelContractNotify;
    // OnContractErrorNotify: TPanelContractErrorNotify;
    
    {*
      Constructor
      @param(APanelID The panel ID.)
    }
    constructor Create(APanelID: Integer);
  end;



  {* Main lightweight DB class }
  TContractDB = class
  private
    FPanelCounter: Integer;
    FSubscribers: specialize TObjectDictionary<Integer, TPanelSubscriber>;
    FTradeRouteLegs: TTradeRouteLegDictionary; // PanelID -> Leg
    FContracts: TContractObjectDictionary; // GroupID -> Contract

    {*
      Get the contract for a GroupID.
      @param(GroupID)
      @returns(TContract)
    }
    function GetContract(const GroupID: Integer): TContract;

    {*
      Get the subscriber for a PanelID.
      @param(PanelID)
      @returns(TPanelSubscriber)
    }
    function GetSubscriber(const PanelID: Integer): TPanelSubscriber;

    {*
      Notify all subscribers for a PanelID.
      @param(PanelID)
      @param(AItem The field that changed.)
      @param(ATradeRouteLeg The updated leg.)
    }
    procedure NotifyPanelLeg(const PanelID: Integer; const AItem: TTradeRouteLegItem; const ATradeRouteLeg: TTradeRouteLeg);
    
    {*
      Notify all subscribers for a GroupID.
      @param(GroupID)
    }
    procedure NotifyContract(const GroupID: Integer);

    {*
      Create a contract if missing or return the existing one.
      @param(GroupID)
      @param(ASCUMaxSize The maximum container size for the new contract)
      @returns(TContract)
    }
    function CreateContractIfMissing(const GroupID: Integer; const ASCUMaxSize: TSCUSize): TContract;

    {*
      Remove a leg from a contract.
      @param(PanelID)
      @param(AContract)
      @returns(@code(True) if the leg was removed.)
    }
    function RemoveLegFromContract(PanelID: Integer; const AContract: TContract): Boolean;
  public
    {*
      Constructor
      @param(AConsoleServer)
    }
    constructor Create(const AConsoleServer: TConsoleReaderThread);

    {* Destructor }
    destructor Destroy; override;

    {* Test the DB. }
    procedure Test;

    {* Set the console server for logging purposes. }
    procedure SetConsoleServer(const AConsoleServer: TConsoleReaderThread);

    {* Unregister a panel and cleanup limbo leg (if any). }
    procedure UnregisterPanel(const PanelID: Integer);

    {*
      Called by panels when something changed in the GUI. The DB will decide
      whether to store the leg in the limbo dict or attach it to a contract.
      AItem describes which field was changed.
      @param(PanelID)
      @param(AItem The field that changed.)
      @param(ATradeRouteLeg The updated leg.)
      @raises(Exception)
    }
    procedure PanelChanged(const PanelID: Integer; const AItem: TTradeRouteLegItem; const ATradeRouteLeg: TTradeRouteLeg);

    {*
      Retrieve all contracts.
      @returns(TContractArray)
    }
    function GetContracts: TContractArray;

    {*
      Register a panel. Returns a unique PanelID. The panel should keep this ID.
      Both callbacks are optional (pass nil if not required).
      @param(ATradeRouteLeg The updated leg.)
      @param(OnLegNotify The callback for leg updates.)
      @param(OnContractNotify The callback for contract updates.)
      @returns(Integer the PanelID.)
    }
    function RegisterPanel( const ATradeRouteLeg: TTradeRouteLeg;
                            const OnLegNotify: TPanelLegNotify;
                            const OnContractNotify: TPanelContractNotify): Integer; overload;

    {*
      Register a panel. Returns a unique PanelID. The panel should keep this ID.
      Both callbacks are optional (pass nil if not required).
      @param(OnLegNotify The callback for leg updates.)
      @param(OnContractNotify The callback for contract updates.)
      @returns(Integer the PanelID.)
    }
    function RegisterPanel( const OnLegNotify: TPanelLegNotify;
                            const OnContractNotify: TPanelContractNotify): Integer; overload;

    {* Retrieve contracts. }
    property Contracts: TContractArray read GetContracts;
  end;




implementation
  
var
  {* Console Server }
  Console: TConsoleReaderThread;



procedure TContractDB.Test;
var
  sub: TPanelSubscriber;
  LegS: String;
begin
  Console.DebugLog('TContractDB.Test', 'Test');
  Console.DebugLog('TContractDB.Test', 'Subscribers: ' + IntToStr(FSubscribers.Count));
  for sub in FSubscribers.Values do
  begin
    if sub.Leg <> nil then
      LegS := '0x' + HexStr(PtrUInt(sub.Leg), SizeOf(Pointer) * 2)
    else
      LegS := 'nil';
    Console.DebugLog('TContractDB.Test', Format('PanelID: %-4d GroupID: %-4d Leg: %s', [sub.PanelID, sub.GroupID, LegS]));
  end;
end;


{ TPanelSubscriber }

constructor TPanelSubscriber.Create(APanelID: Integer);
begin
  inherited Create;
  PanelID := APanelID;
  GroupID := 0;
  Leg := nil;
  OnLegNotify := nil;
  OnContractNotify := nil;
  Console.DebugLog('TPanelSubscriber.Create', ' PanelID: ' + IntToStr(APanelID));
end;



{ TContractDB }

constructor TContractDB.Create(const AConsoleServer: TConsoleReaderThread);
begin
  inherited Create;
  Console := AConsoleServer;
  TContract.SetConsoleServer(AConsoleServer);
  FPanelCounter := 0;
  FSubscribers    := specialize TObjectDictionary<Integer, TPanelSubscriber>.Create([doOwnsValues]);
  FTradeRouteLegs := specialize TDictionary<Integer, TTradeRouteLeg>.Create;
  FContracts      := TContractObjectDictionary.Create([doOwnsValues]);
end;



destructor TContractDB.Destroy;
var
  i: Integer;
  kv: specialize TPair<Integer, TContract>;
  Leg: TContractLeg;
  Contract: TContract;
  ContractLegArray: TContractLegArray;
begin
  Console.DebugLog('TContractDB.Destroy', 'STARTED');
  Console.DebugLog('TContractDB.Destroy', 'FContracts count: ' + IntToStr(FContracts.Count));
  // Debug degli indirizzi di memoria dei contratti
  for kv in FContracts do
  begin
    Console.DebugLog('TContractDB.Destroy', 'Contract GroupID: ' + IntToStr(kv.Key) + ' Address: ' + IntToHex(NativeInt(kv.Value), 8));
  end;

  FContracts.Free;
  Console.DebugLog('TContractDB.Destroy', 'FContracts freed');
  FTradeRouteLegs.Free;
  Console.DebugLog('TContractDB.Destroy', 'FTradeRouteLegs freed');
  FSubscribers.Free;
  Console.DebugLog('TContractDB.Destroy', 'FSubscribers freed');

  inherited Destroy;
  Console.DebugLog('TContractDB.Destroy', 'COMPLETED');
end;



procedure TContractDB.SetConsoleServer(const AConsoleServer: TConsoleReaderThread);
begin
  Console := AConsoleServer;
end;



function TContractDB.GetSubscriber(const PanelID: Integer): TPanelSubscriber;
begin
  Result := nil;
  if (PanelID >= 0) then
    FSubscribers.TryGetValue(PanelID, Result);
end;



function TContractDB.RegisterPanel( const ATradeRouteLeg: TTradeRouteLeg;
                                    const OnLegNotify: TPanelLegNotify;
                                    const OnContractNotify: TPanelContractNotify): Integer;
var                
  TradeRouteLeg: TTradeRouteLeg;
  sub: TPanelSubscriber;
begin
  Inc(FPanelCounter);
  Console.DebugLog('TContractDB.RegisterPanel', 'Registering new panel with PanelID: ' + IntToStr(FPanelCounter));
  sub := TPanelSubscriber.Create(FPanelCounter);
  sub.OnLegNotify := OnLegNotify;
  sub.OnContractNotify := OnContractNotify;
  FSubscribers.Add(FPanelCounter, sub);

  // create an empty limbo leg for this panel
  TradeRouteLeg := ATradeRouteLeg;
  TradeRouteLeg.GroupId := 0;
  FTradeRouteLegs.Add(FPanelCounter, TradeRouteLeg);

  Console.DebugLog('TContractDB.RegisterPanel', 'TTradeRouteLeg added to limbo for PanelID: ' + IntToStr(FPanelCounter));
  Result := FPanelCounter;
end;



function TContractDB.RegisterPanel( const OnLegNotify: TPanelLegNotify;
                                    const OnContractNotify: TPanelContractNotify): Integer;
var
  defaultLeg: TTradeRouteLeg;
begin
  defaultLeg := default(TTradeRouteLeg);
  Result := RegisterPanel(defaultLeg, OnLegNotify, OnContractNotify);
end;



procedure TContractDB.UnregisterPanel(const PanelID: Integer);
var
  sub: TPanelSubscriber;
  contract: TContract;
  GroupID: Integer;
begin
  sub := GetSubscriber(PanelID);
  if sub = nil then Exit;
  GroupID := sub.GroupID;

  Console.DebugLog('TContractDB.UnregisterPanel', 'Unregistering panel with PanelID: ' + IntToStr(PanelID));

  // If panel was attached to a contract, try to remove its leg from that contract
  if FContracts.TryGetValue(sub.GroupID, contract) then
  begin
    Console.DebugLog('TContractDB.UnregisterPanel', 'GroupID <> 0 and contract <> nil');
    // if RemoveLegFromContract(contract, sub.Leg) then
    if RemoveLegFromContract(PanelID, contract) then
    begin
      sub.Leg.Free;
      Console.DebugLog('TContractDB.UnregisterPanel', 'Leg freed for PanelID: ' + IntToStr(PanelID));

      if contract.ContractLegsCount = 0 then
      begin
        Console.DebugLog('TContractDB.UnregisterPanel', 'Contract now empty: freeing GroupID: ' + IntToStr(sub.GroupID));
        FContracts.Remove(sub.GroupID);
        contract := nil;
      end;
    end;
    sub.Leg := nil;
  end;

  // remove limbo leg if present
  if FTradeRouteLegs.ContainsKey(PanelID) then
  begin
    Console.DebugLog('TContractDB.UnregisterPanel', 'Removing limbo leg for PanelID: ' + IntToStr(PanelID));
    FTradeRouteLegs.Remove(PanelID);
  end;

  FSubscribers.Remove(PanelID);    
  NotifyContract(GroupID);
end;



function TContractDB.CreateContractIfMissing(const GroupID: Integer; const ASCUMaxSize: TSCUSize): TContract;
begin
  Result := nil;
  if not FContracts.TryGetValue(GroupID, Result) then
  begin
    Result := TContract.Create(GroupID, ASCUMaxSize);
    // minimal init: contract properties could be inferred later
    FContracts.Add(GroupID, Result);  
    Console.DebugLog('TContractDB.CreateContractIfMissing', 'created: ' + IntToStr(GroupID));
  end;
end;



function TContractDB.GetContract(const GroupID: Integer): TContract;
begin
  if not FContracts.TryGetValue(GroupID, Result) then
    Result := nil;
end;



function TContractDB.RemoveLegFromContract(PanelID: Integer; const AContract: TContract): Boolean;
var
  Leg: TContractLeg;
begin
  Result := False;
  Console.DebugLog('TContractDB.RemoveLegFromContract', 'PanelID: ' + IntToStr(PanelID) + ' GroupID: ' + IntToStr(AContract.ID));
  AContract.RemoveTradeRouteLeg(PanelID, Leg);
  if (Leg <> nil) and (Leg.UnloadingsCount = 0) then
    Result := AContract.RemoveContractLeg(Leg);
end;



procedure TContractDB.NotifyPanelLeg(const PanelID: Integer; const AItem: TTradeRouteLegItem; const ATradeRouteLeg: TTradeRouteLeg);
var
  sub: TPanelSubscriber;
begin
  sub := GetSubscriber(PanelID);
  if (sub <> nil) and Assigned(sub.OnLegNotify) then
    sub.OnLegNotify(Self, PanelID, AItem, ATradeRouteLeg);
end;



procedure TContractDB.NotifyContract(const GroupID: Integer);
var
  pair: specialize TPair<Integer, TPanelSubscriber>;
  contract: TContract;
begin
  if not FContracts.TryGetValue(GroupID, contract) then Exit;

  // notify all subscribers that have GroupID equal to this one
  for pair in FSubscribers do
  begin
    if pair.Value.GroupID = GroupID then
    begin
      if Assigned(pair.Value.OnContractNotify) then
        pair.Value.OnContractNotify(Self, GroupID, contract);
    end;
  end;
end;



procedure TContractDB.PanelChanged(const PanelID: Integer; const AItem: TTradeRouteLegItem; const ATradeRouteLeg: TTradeRouteLeg);
var
  sub: TPanelSubscriber;
  currentGroup: Integer;
  contract: TContract;
  ContractLeg: TContractLeg;
  previousGroup: Integer;
  TradeRouteLeg: TTradeRouteLeg;
  FormSizeDialog: TFormSizeDialog;
  SelectedValue: Integer;
  ID: Integer;
  IDSet: TIDSet;
  ItemError: TTradeRouteLegItemError;
  LegNewed: Boolean;
begin
  TradeRouteLeg := Default(TTradeRouteLeg);

  Console.DebugLog('TContractDB.PanelChanged', 'PanelID: ' + IntToStr(PanelID) + ' AItem: ' + TradeRouteLegItemArray[AItem] + ' Leg.GroupId: ' + IntToStr(ATradeRouteLeg.GroupId));
  sub := GetSubscriber(PanelID);
  if sub = nil then Exit;

  previousGroup := sub.GroupID;
  currentGroup := ATradeRouteLeg.GroupId;



  Console.DebugLog('TContractDB.PanelChanged', 'Subscriber found. Previous GroupID: ' + IntToStr(sub.GroupID) + ' New GroupID: ' + IntToStr(ATradeRouteLeg.GroupId));
  case AItem of
    eGroupId: begin

      Console.DebugLog('TContractDB.PanelChanged', 'eGroupId');

      {
        - currentGroup = 0                       A00
          - previousGroup = 0                    A10
          - previousGroup <> 0                   A20
        - currentGroup <> 0                      B00
          - previousGroup = 0                    B10
          - previousGroup <> 0                   B20
            - currentGroup = previousGroup       B21
            - currentGroup <> previousGroup      B22
      }

      if previousGroup = currentGroup then
      begin
        Console.DebugLog('TContractDB.PanelChanged', 'No GroupID change detected. Exiting.');
        Exit;                                                                                            {A10} {B21}
      end;

      if currentGroup = 0 then                                                                           {A00}
      begin
        Console.DebugLog('TContractDB.PanelChanged', 'New GroupID is 0. Updating mapping for this panel.');
        
        Console.DebugLog('TContractDB.PanelChanged', 'Storing leg in limbo for PanelID: ' + IntToStr(PanelID));
        FTradeRouteLegs.AddOrSetValue(PanelID, ATradeRouteLeg);
        sub.GroupID := 0;

        // previousGroup <> 0                                                                            {A20}
        Console.DebugLog('TContractDB.PanelChanged', 'Previous GroupID <> 0. Attempting to remove leg from contract.');
        if FContracts.TryGetValue(previousGroup, contract) then
        begin
          Console.DebugLog('TContractDB.PanelChanged', 'Found previous contract. Removing leg.');
          if RemoveLegFromContract(PanelID, contract) then
          begin
            Console.DebugLog('TContractDB.PanelChanged', 'Leg removed from contract for PanelID: ' + IntToStr(PanelID));
            sub.Leg.Free;

            if contract.ContractLegsCount = 0 then
            begin
              Console.DebugLog('TContractDB.PanelChanged', 'Contract now empty: freeing GroupID: ' + IntToStr(previousGroup));
              FContracts.Remove(previousGroup);
              contract := nil;
            end;
          end;
          NotifyContract(previousGroup);
          sub.Leg := nil;
        end;
      end
      else                                                                                               {B00}
      begin
        if (previousGroup <> 0) then                                                                     {B20} {B22}
        begin
          Console.DebugLog('TContractDB.PanelChanged', 'Previous GroupID <> 0. Attempting to remove leg from previous contract.');
          if FContracts.TryGetValue(previousGroup, contract) then
          begin
            Console.DebugLog('TContractDB.PanelChanged', 'Found previous contract. Removing leg.');
            if RemoveLegFromContract(PanelID, contract) then
            begin
              sub.Leg.Free;
              Console.DebugLog('TContractDB.PanelChanged', 'Leg removed from previous contract for PanelID: ' + IntToStr(PanelID));
            end;

            if contract.ContractLegsCount = 0 then
            begin
              Console.DebugLog('TContractDB.PanelChanged', 'Previous contract now empty: freeing GroupID: ' + IntToStr(previousGroup));
              FContracts.Remove(previousGroup);
              contract := nil;
            end;
            sub.Leg := nil;
          end;
        end;

        // Leg is in limbo

        if TContractLeg.IsConformedTradeRouteLeg(ATradeRouteLeg) then
        begin
          Console.DebugLog('TContractDB.PanelChanged', 'Leg is conforming: adding to contract.');

          contract := CreateContractIfMissing(currentGroup, ATradeRouteLeg.SCUMaxSize);

          LegNewed := False;

          sub.Leg := contract.GetContractLeg(ATradeRouteLeg.LoadingStationName, ATradeRouteLeg.Commodity);

          if sub.Leg = nil then
          begin
            Console.DebugLog('TContractDB.PanelChanged', 'sub.Leg = nil -> creating new TContractLeg');
            try
              sub.Leg := TContractLeg.Create(ATradeRouteLeg.LoadingStationName, ATradeRouteLeg.Commodity, ATradeRouteLeg.SCUMaxSize);
            except
              on E: Exception do
              begin
                Console.DebugLog('TContractDB.PanelChanged', 'Exception caught while creating TContractLeg for PanelID: ' + IntToStr(PanelID));
                raise;
              end;
            end;
            LegNewed := True;
          end;

          ItemError := erNone;
          if contract.SCUMaxSize <> ATradeRouteLeg.SCUMaxSize then
            ItemError := erSCUMaxSize
          else
            sub.Leg.AddOrSetTradeRouteLeg(PanelID, ATradeRouteLeg, ItemError);

          case ItemError of
            erSCUMaxSize:
            begin
              FormSizeDialog := TFormSizeDialog.Create(nil, SCUSizeIntArray);
              IDSet := TIDSet.Create;

              try
                ShowMessage('The selected SCU Max Size is not valid for this contract leg.' + LineEnding + LineEnding +
                            'Please select a valid SCU Max Size for the contract.' + LineEnding + LineEnding +
                            'All legs in a contract must have the same SCU Max Size.');

                if FormSizeDialog.ShowModal = mrOK then
                begin
                  if LegNewed then
                  begin
                    sub.Leg.Free;
                  end;
                  sub.Leg := nil;

                  SelectedValue := FormSizeDialog.SelectedSize;

                  TradeRouteLeg.SCUMaxSize := TSCUxSizeRecord.GetSizeValue(SelectedValue);
                  TradeRouteLeg.GroupId := 0;

                  for ContractLeg in contract.ContractLegs do
                  begin
                    for ID in ContractLeg.GetTradeRouteLegIDs do
                    begin
                      Console.DebugLog('TContractDB.PanelChanged', 'Adding ID: ' + IntToStr(ID));
                      IDSet.Add(ID);
                    end;
                  end;

                  IDSet.Add(PanelID);

                  for ID in IDSet do
                  begin
                    NotifyPanelLeg(ID, eGroupId, TradeRouteLeg);
                  end;

                  //! here the contract should have been deleted
                  //! all ContractLegs of the contract must have been released

                  for ID in IDSet do
                  begin
                    NotifyPanelLeg(ID, eSCUMaxSize, TradeRouteLeg);
                  end;

                  TradeRouteLeg.GroupId := currentGroup;
                  for ID in IDSet do
                  begin
                    NotifyPanelLeg(ID, eGroupId, TradeRouteLeg);
                  end;
                  Exit;
                end
                else begin
                  Console.DebugLog('TContractDB.PanelChanged', 'Resetting GroupID for PanelID: ' + IntToStr(PanelID));
                  TradeRouteLeg.GroupId := 0;     
                  if LegNewed then
                  begin
                    sub.Leg.Free;
                  end;
                  sub.Leg := nil;
                  NotifyPanelLeg(PanelID, eGroupId, TradeRouteLeg);
                  Exit;
                end;
              finally
                FormSizeDialog.Free;
                IDSet.Free;
              end;
            end;

            erLoadingStationName:
            begin
              ShowMessage('Internal Error: The selected Loading Station is already used in another leg of this contract.' + LineEnding + LineEnding +
                            'Please report this error to the developer. Thank you.');
              TradeRouteLeg.GroupId := 0;
              if LegNewed then
              begin
                sub.Leg.Free;
              end;
              sub.Leg := nil;
              NotifyPanelLeg(PanelID, eGroupId, TradeRouteLeg);
              Exit;
            end;

            erCommodity:
            begin
              ShowMessage('Internal Error: The selected Loading Station is already used in another leg of this contract.' + LineEnding + LineEnding +
                            'Please report this error to the developer. Thank you.');
              TradeRouteLeg.GroupId := 0;
              if LegNewed then
              begin
                sub.Leg.Free;
              end;
              sub.Leg := nil;
              NotifyPanelLeg(PanelID, eGroupId, TradeRouteLeg);
              Exit;
            end;

            erUnloadingStationName:
            begin
              ShowMessage('The selected Unloading Station is already used in another leg of this contract.' + LineEnding + LineEnding +
                            'Please select a different Unloading Station for this leg.');
              TradeRouteLeg.GroupId := 0;
              if LegNewed then
              begin
                sub.Leg.Free;
              end;
              sub.Leg := nil;
              NotifyPanelLeg(PanelID, eGroupId, TradeRouteLeg);
              Exit;
            end;
          end;

          contract.AddContractLeg(sub.Leg);

          // Remove limbo mapping (if any)
          if FTradeRouteLegs.ContainsKey(PanelID) then
          begin
            Console.DebugLog('TContractDB.PanelChanged', 'Removing limbo leg for PanelID: ' + IntToStr(PanelID));
            FTradeRouteLegs.Remove(PanelID);
          end;

          Console.DebugLog('TContractDB.PanelChanged', 'Updating subscriber info for PanelID: ' + IntToStr(PanelID));
          sub.GroupID := currentGroup;
        end;

        // Notify all panels bound to this GroupID
        NotifyContract(currentGroup);
      end;
    end;
    eLoadingStationName: begin
      if currentGroup = 0 then
      begin
        Console.DebugLog('TContractDB.PanelChanged', 'eLoadingStationName: currentGroup = 0');
        FTradeRouteLegs.AddOrSetValue(PanelID, ATradeRouteLeg);
        NotifyPanelLeg(PanelID, eIgnored, ATradeRouteLeg);
      end
      else begin
        TradeRouteLeg.GroupId := 0;
        NotifyPanelLeg(PanelID, eGroupId, TradeRouteLeg);
        NotifyPanelLeg(PanelID, eGroupId, ATradeRouteLeg);
      end;
    end;
    eUnloadingStationName: begin
      if currentGroup = 0 then
      begin
        Console.DebugLog('TContractDB.PanelChanged', 'eUnloadingStationName: currentGroup = 0');
        FTradeRouteLegs.AddOrSetValue(PanelID, ATradeRouteLeg);
        NotifyPanelLeg(PanelID, eIgnored, ATradeRouteLeg);
      end
      else begin
        TradeRouteLeg.GroupId := 0;
        NotifyPanelLeg(PanelID, eGroupId, TradeRouteLeg);
        NotifyPanelLeg(PanelID, eGroupId, ATradeRouteLeg);
      end;
    end;
    eCommodity: begin
      if currentGroup = 0 then
      begin
        Console.DebugLog('TContractDB.PanelChanged', 'eCommodity: currentGroup = 0');
        FTradeRouteLegs.AddOrSetValue(PanelID, ATradeRouteLeg);
        NotifyPanelLeg(PanelID, eIgnored, ATradeRouteLeg);
      end
      else begin
        TradeRouteLeg.GroupId := 0;
        NotifyPanelLeg(PanelID, eGroupId, TradeRouteLeg);
        NotifyPanelLeg(PanelID, eGroupId, ATradeRouteLeg);
      end;
    end;
    eSCU: begin
      if currentGroup = 0 then
      begin
        Console.DebugLog('TContractDB.PanelChanged', 'eSCU: currentGroup = 0');
        FTradeRouteLegs.AddOrSetValue(PanelID, ATradeRouteLeg);
        NotifyPanelLeg(PanelID, eIgnored, ATradeRouteLeg);
      end
      else begin
        TradeRouteLeg.GroupId := 0;
        NotifyPanelLeg(PanelID, eGroupId, TradeRouteLeg);
        NotifyPanelLeg(PanelID, eGroupId, ATradeRouteLeg);
      end;
    end;
    eSCUMaxSize: begin
      if currentGroup = 0 then
      begin
        Console.DebugLog('TContractDB.PanelChanged', 'eSCUMaxSize: currentGroup = 0');
        FTradeRouteLegs.AddOrSetValue(PanelID, ATradeRouteLeg);
        NotifyPanelLeg(PanelID, eIgnored, ATradeRouteLeg);
      end
      else begin
        TradeRouteLeg.GroupId := 0;
        NotifyPanelLeg(PanelID, eGroupId, TradeRouteLeg);  
        NotifyPanelLeg(PanelID, eSCUMaxSize, ATradeRouteLeg);
        NotifyPanelLeg(PanelID, eGroupId, ATradeRouteLeg);
      end;
    end;
    eDone: begin
      if currentGroup = 0 then
      begin
        Console.DebugLog('TContractDB.PanelChanged', 'eDone: currentGroup = 0');
        FTradeRouteLegs.AddOrSetValue(PanelID, ATradeRouteLeg);
        NotifyPanelLeg(PanelID, eIgnored, ATradeRouteLeg);
      end
      else begin
        TradeRouteLeg.GroupId := 0;
        NotifyPanelLeg(PanelID, eGroupId, TradeRouteLeg);
        NotifyPanelLeg(PanelID, eGroupId, ATradeRouteLeg);
      end;
    end;
    eHide: begin
      if currentGroup = 0 then
      begin
        Console.DebugLog('TContractDB.PanelChanged', 'eHide: currentGroup = 0');
        FTradeRouteLegs.AddOrSetValue(PanelID, ATradeRouteLeg);
        NotifyPanelLeg(PanelID, eIgnored, ATradeRouteLeg);
      end
      else begin
        TradeRouteLeg.GroupId := 0;
        NotifyPanelLeg(PanelID, eGroupId, TradeRouteLeg);
        NotifyPanelLeg(PanelID, eGroupId, ATradeRouteLeg);
      end;
    end;
  else ;
  end;
end;



function TContractDB.GetContracts: TContractArray;
begin
  Result := FContracts.Values.ToArray;
end;




initialization



finalization



end.

