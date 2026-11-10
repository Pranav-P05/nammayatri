UPDATE atlas_driver_offer_bpp.transporter_config
SET driver_wallet_config = jsonb_set(
  driver_wallet_config::jsonb,
  '{enableDriverWallet}', 'true'
)
WHERE merchant_operating_city_id IN (SELECT id FROM atlas_driver_offer_bpp.merchant_operating_city WHERE merchant_short_id = 'BHARAT_TAXI_PARTNER' AND city = 'Delhi');

UPDATE atlas_driver_offer_bpp.transporter_config
SET driver_wallet_config = jsonb_set(
  driver_wallet_config::jsonb,
  '{onlineCommissionPaidOutDirectly}', 'true'
)
WHERE merchant_operating_city_id IN (SELECT id FROM atlas_driver_offer_bpp.merchant_operating_city WHERE merchant_short_id = 'BHARAT_TAXI_PARTNER' AND city = 'Delhi');
