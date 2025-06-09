module Test.MySolutions where

import Prelude

import Data.AddressBook (AddressBook, Entry, findEntry)
import Data.List (filter, head, nubByEq)
import Data.Maybe (Maybe, isJust)

findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street =
  head <<< filter (\entry -> entry.address.street == street)

isInBook :: String -> String -> AddressBook -> Boolean
isInBook fname lname book = isJust $ findEntry fname lname book

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubByEq duplicatedEntries
  where
  duplicatedEntries :: Entry -> Entry -> Boolean
  duplicatedEntries e1 e2 = e1.firstName == e2.firstName && e1.lastName == e2.lastName
