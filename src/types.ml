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

type dkim_verification_status = Pending | Success | Failed | TemporaryFailure

type dkim_attributes = {
  dkim_enabled : bool ;
  dkim_tokens : string list ;
  dkim_verification_status : dkim_verification_status ;
}

type dkim = {
  dkim_key : string ;
  dkim_attributes : dkim_attributes ;
}