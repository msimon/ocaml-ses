type creds = {
  aws_key: string ;
  aws_secret: string ;
}

type send_data_points = {
  bounce: int ;
  complaints: int ;
  delivery_attempts: int ;
  rejects: int ;
  timestamp: float ;
}

type destination = {
  bcc_addresses: string list ;
  cc_addresses: string list ;
  to_addresses: string list;
}

type content = {
  charset: string;
  data: string;
}

type body = {
  html: content;
  text: content;
}

type message = {
  subject: content;
  body: body;
}

type verification_status = Pending | Success | Failed | TemporaryFailure | NotStarted

type dkim_attributes = {
  dkim_enabled : bool ;
  dkim_tokens : string list ;
  dkim_verification_status : verification_status ;
}

type notification_attributes = {
  forwarding_enable: bool ;
  bounce_topic : string ;
  complaint_topic : string ;
}

type verification_attributes = {
  verification_status : verification_status ;
  verification_token: string ;
}

type value =
  | Verification of verification_attributes
  | Notification of notification_attributes
  | Dkim of dkim_attributes

type entry = {
  key : string ;
  value: value ;
}
