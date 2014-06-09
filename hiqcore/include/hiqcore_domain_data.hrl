-ifndef(hiqcore_store_data_hrl).
-define(hiqcore_store_data_hrl, true).

% hiqmail DB
-record(user, {'_id', insertDate, updateDate, status,
               user, password, email, name, lastName, phone}).

-record(mailRecipient, {'_id', insertDate, updateDate, status,
                        email, lastName, birthDate, name}).

-record(mailList, {'_id', insertDate, updateDate, status,
                   userId, name}).

-record(mailRecipientList, {'_id', insertDate, updateDate, status,
                            mailListId, recipientId}).

-record(mailUserList, {'_id', insertDate, updateDate, status,
                       mailListId, userId}).

-record(mailFilter, {'_id', insertDate, updateDate, status,
                     name, encodedRules}).

-record(mailUserFilter, {'_id', insertDate, updateDate, status,
                         userId, filterId, priority}).

-record(mailHeader, {'_id', insertDate, updateDate, status,
                     keyValueList}).

-record(mailBody, {'_id', insertDate, updateDate, status,
                   userId, mailHeaderId, mailAttachmentList, template}).

-record(mailAttachment, {'_id', insertDate, updateDate, status,
                         name, dataReference}).

-record(mailStatus, {'_id', insertDate, updateDate, status,
                     recipientId, mailDeliveryId}).

-record(mailDelivery, {'_id', insertDate, updateDate, status,
                       userId, startTime, endTime, mailListId,
                       mailBodyId, mailCampaignId, mailTagList}).

-record(mailCampaign, {'_id', insertDate, updateDate, status,
                       name}).

-record(mailTag, {'_id', insertDate, updateDate, status,
                  name}).

% hiqmail stats DB
-record(mailView, {'_id', insertDate,
                   mailDeliveryId, recipientId, browser}).

-record(mailClick, {'_id', insertDate,
                    mailDeliveryId, recipientId, country}).

-endif.
